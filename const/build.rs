//! Build script to auto-generate const definitions from the Bluetooth SIG
//! assigned numbers [YAML repository].
//!
//! The `generate` feature must be enabled and `BUILD_SRC` file removed to
//! re-generate file content. This is done to track changes and to ensure that
//! regular builds do not access the network. Generated files are written to
//! `src/` and should only be committed after a manual review.
//!
//! [YAML repository]: https://bitbucket.org/bluetooth-SIG/public/

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    #[cfg(feature = "generate")]
    generate::main();
}

#[cfg(feature = "generate")]
mod generate {
    use std::borrow::Cow;
    use std::fmt::Write;
    use std::path::{Component, Path};
    use std::process::{Command, Stdio};
    use std::{env, fs, io, thread};

    type Zip = zip::ZipArchive<io::Cursor<Vec<u8>>>;

    pub(super) fn main() {
        let (name, mut zip) = fetch("https://bitbucket.org/bluetooth-SIG/public/get/main.zip");
        gen_company_ids(&mut zip, "src/company_ids.rs");
        gen_uuids(&mut zip, "src/uuid16.rs");

        println!("cargo:rerun-if-changed=BUILD_SRC");
        fs::write("BUILD_SRC", name + "\n").unwrap();
    }

    fn gen_company_ids(zip: &mut Zip, out: &str) {
        #[derive(serde::Deserialize)]
        struct CompanyId {
            value: u16,
            name: String,
        }
        let ids = {
            #[derive(serde::Deserialize)]
            struct File {
                company_identifiers: Vec<CompanyId>,
            }
            let mut f: File = read(zip, "company_identifiers/company_identifiers.yaml");
            f.company_identifiers.sort_unstable_by_key(|c| c.value);
            f.company_identifiers
        };

        let mut tab = String::with_capacity(2 * ids.iter().fold(0, |n, id| n + id.name.len()));
        let mut end = Vec::<u16>::with_capacity(ids.len());
        let mut off = 0;
        for (i, id) in ids.iter().enumerate() {
            assert_eq!(usize::from(id.value), i);
            write!(&mut tab, "\\\n        {}", id.name.escape_debug()).unwrap();
            off += id.name.len();
            // When this fails, the table should be split up
            end.push(u16::try_from(off).expect("u16 overflow"));
        }
        tab.push_str("\\\n    ");

        let (max, last) = ids.last().map(|id| (id.value, &id.name)).unwrap();
        let buf = format!(
            r#"
            use super::CompanyId;

            impl CompanyId {{
                pub(super) const TAB: &str = "{tab}";
                pub(super) const END: [u16; {max} + 1] = {end:?};
            }}

            #[cfg(test)]
            mod tests {{
                #[test]
                fn company_ids() {{
                    use super::CompanyId;
                    assert_eq!(CompanyId(0).name(), Some("Ericsson AB"));
                    assert_eq!(CompanyId(0x0600).name(), Some("iRobot Corporation"));
                    assert_eq!(CompanyId({max:#06X}).name(), Some("{last}"));
                    assert_eq!(CompanyId({max:#06X} + 1).name(), None);
                    assert_eq!(CompanyId(u16::MAX).name(), None);
                }}
            }}
            "#
        );
        write(out, buf);
    }

    fn gen_uuids(zip: &mut Zip, out: &str) {
        #[derive(serde::Deserialize)]
        struct Uuid {
            uuid: u16,
            name: String,
            id: String,
        }
        fn key(u: &Uuid) -> (u16, &str) {
            u.id.rsplit_once('.').map(|(_, id)| (u.uuid, id)).unwrap()
        }

        let mut buf = String::with_capacity(64 * 1024);
        let mut gen = |doc: &str, name: &str, path: &str, map: fn(Uuid) -> Cow<'static, str>| {
            #[derive(serde::Deserialize)]
            struct File {
                uuids: Vec<Uuid>,
            }
            let mut f: File = read(zip, path);
            f.uuids.sort_unstable_by_key(|v| v.uuid);
            if !buf.is_empty() {
                buf.push('\n');
            }
            write!(
                buf,
                "uuid16_enum! {{\n    /// {doc}\n    pub enum {name} {{"
            )
            .unwrap();
            for u in f.uuids {
                let (uuid, name) = (u.uuid, heck::AsUpperCamelCase(map(u)));
                if !name.0.is_empty() {
                    write!(buf, "\n        {name} = {uuid:#06X},").unwrap();
                }
            }
            buf.push_str("\n    }\n}\n");
        };
        let from_name = |u: Uuid| Cow::from(u.name);

        gen(
            "SDP service class identifiers ([Assigned Numbers] Section 3.3).",
            "ServiceClass",
            "assigned_numbers/uuids/service_class.yaml",
            |u| {
                Cow::from(match key(&u) {
                    (0x1000, "service_discovery_server") => "ServiceDiscoveryServer",
                    (0x1001, "browse_group_descriptor") => "BrowseGroupDescriptor",
                    (0x1137, "3d_display") => "ThreeDDisplay",
                    (0x1138, "3d_glasses") => "ThreeDGlasses",
                    (0x110D, "a2dp")
                    | (0x111A, "bip")
                    | (0x1122, "bpp")
                    | (0x1125, "hcrp")
                    | (0x1130, "phone_book_access")
                    | (0x1134, "message_access_profile")
                    | (0x1135, "global_navigiation_satellite_system")
                    | (0x1139, "3d_synchronization")
                    | (0x113A, "multi_profile")
                    | (0x113E, "calendar_task_notes")
                    | (0x1305, "video_distribution")
                    | (0x1400, "health_device") => "",
                    _ => return u.name.into(),
                })
            },
        );
        gen(
            "GATT services ([Assigned Numbers] Section 3.4.2).",
            "Service",
            "assigned_numbers/uuids/service_uuids.yaml",
            from_name,
        );
        gen(
            "Characteristic presentation format units ([Assigned Numbers] Section 3.5.2).",
            "Unit",
            "assigned_numbers/uuids/units.yaml",
            |u| {
                let (uuid, id) = key(&u);
                if let Some(i) = id.find("_per_") {
                    return Cow::from(match (uuid, id) {
                        (0x271B, "kilogram_per_cubic_metre") => {
                            "ConcentrationKilogramsPerCubicMetre"
                        }
                        (0x2745, "watt_per_square_metre") => "FluxWattsPerSquareMetre",
                        (0x274E, "coulomb_per_square_metre") => "FluxCoulombsPerSquareMetre",
                        (0x2750, "henry_per_metre") => "HenriesPerMetre",
                        (0x27A5, "pound_force_per_square_inch") => "PoundsPerSquareInch",
                        (0x27BD, "kilometer_per_minute") => "KilometresPerMinute",
                        _ if id.as_bytes()[i - 1] != b's' => {
                            return id.replacen("_per_", "s_per_", 1).into()
                        }
                        _ => return id.to_owned().into(),
                    });
                }
                Cow::from(match (uuid, id) {
                    (0x2700, "unitless") => "None",
                    (0x271E, "relative_permeability") => "RelativePermeability",
                    (0x272D, "tesla") => "Tesla",
                    (0x272E, "henry") => "Henries",
                    (0x272F, "degree_celsius") => "Celsius",
                    (0x2764, "minute") => "DegreeMinutes",
                    (0x2765, "second") => "DegreeSeconds",
                    (0x2781, "millimetre_of_mercury") => "MillimetresOfMercury",
                    (0x2782, "ångström") => "Angstroms",
                    (0x27A2, "inch") => "Inches",
                    (0x27A3, "foot") => "Feet",
                    (0x27AC, "degree_fahrenheit") => "Fahrenheit",
                    (0x27AD, "percentage") => "Percent",
                    (0x27AE, "per_mille") => "PartsPerThousand",
                    (0x27B9, "metabolic_equivalent") => "MetabolicEquivalent",
                    (0x27C3, "decibel_spl") => "Decibels",
                    (0x27C4, "ppm") => "PartsPerMillion",
                    (0x27C5, "ppb") => "PartsPerBillion",
                    _ if !b"sxz".contains(id.as_bytes().last().unwrap()) => {
                        return (id.to_owned() + "s").into()
                    }
                    _ => return id.to_owned().into(),
                })
            },
        );
        gen(
            "Declarations ([Assigned Numbers] Section 3.6).",
            "Declaration",
            "assigned_numbers/uuids/declarations.yaml",
            from_name,
        );
        gen(
            "Descriptors ([Assigned Numbers] Section 3.7).",
            "Descriptor",
            "assigned_numbers/uuids/descriptors.yaml",
            from_name,
        );
        gen(
            "Characteristics ([Assigned Numbers] Section 3.8.2).",
            "Characteristic",
            "assigned_numbers/uuids/characteristic_uuids.yaml",
            |u| {
                Cow::from(match key(&u) {
                    (0x2A2A, "ieee_11073-20601_regulatory_certification_data_list") => {
                        "IeeeRegulatoryCertificationDataList"
                    }
                    (0x2A50, "pnp_id") => "PnpId",
                    (0x2AA0, "magnetic_flux_density_2d") => "MagneticFluxDensity2D",
                    (0x2AA1, "magnetic_flux_density_3d") => "MagneticFluxDensity3D",
                    (0x2AE7, "cie_13_3_1995_color_rendering_index") => "CieColorRenderingIndex",
                    (0x2B04, "percent_8") => "Percent8",
                    _ => return u.name.into(),
                })
            },
        );
        write(out, buf);
    }

    fn fetch(url: &str) -> (String, Zip) {
        use std::io::Read;
        let rsp = (ureq::get(url).call()).expect("failed to download file");
        assert_eq!(rsp.content_type(), "application/zip");
        let name = (rsp.header("content-disposition"))
            .and_then(|v| v.split_once("filename=").map(|(_, name)| name))
            .expect("invalid content-disposition header")
            .to_owned();
        let len = (rsp.header("content-length")).expect("missing content-length header");
        let mut buf = Vec::with_capacity(len.parse().expect("invalid content-length header"));
        (rsp.into_reader().take(8 * 1024 * 1024))
            .read_to_end(&mut buf)
            .expect("failed to download file");
        (
            name,
            Zip::new(io::Cursor::new(buf)).expect("invalid zip file"),
        )
    }

    fn read<T: serde::de::DeserializeOwned>(zip: &mut Zip, path: &str) -> T {
        let first = Path::new(zip.file_names().next().expect("empty zip file"));
        let root = (first.components().find_map(|c| match c {
            Component::Normal(root) => Some(root.to_str().unwrap().to_owned()),
            _ => None,
        }))
        .expect("failed to determine zip root");
        let r = (zip.by_name(&(root + "/" + path))).expect("file not found");
        serde_yaml::from_reader(r).expect("failed to parse yaml file")
    }

    fn write(path: &str, mut buf: String) {
        println!("cargo:rerun-if-changed={path}");
        buf.insert_str(0, "// Code generated by build.rs. DO NOT EDIT.\n\n");
        fs::write(path, rustfmt(buf)).expect("failed to write file");
    }

    fn rustfmt(src: String) -> Vec<u8> {
        let mut rustfmt = if let Ok(rustfmt) = env::var("RUSTFMT") {
            Command::new(rustfmt)
        } else {
            Command::new("rustfmt")
        };
        let mut child = rustfmt
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit())
            .spawn()
            .expect("failed to spawn rustfmt");
        let mut stdin = child.stdin.take().unwrap();
        let handle = thread::spawn(move || io::Write::write_all(&mut stdin, src.as_bytes()));
        let out = child.wait_with_output().expect("failed to wait on rustfmt");
        (handle.join().expect("stdin thread panic")).expect("failed to write to rustfmt");
        assert!(out.status.success(), "rustfmt error");
        out.stdout
    }
}
