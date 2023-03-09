Blackrock User-Mode Bluetooth LE Library
========================================

A cross-platform user-mode BLE stack implementation starting from the USB transport layer via [libusb].

[libusb]: https://github.com/libusb/libusb

**Project status:** Under active development. Tested on Windows and Linux. Not accepting external contributions at this time. The library implements all components for a BLE GATT server (peripheral role) and LE Secure Connections pairing. Minimum supported Bluetooth version is 5.0. All APIs are subject to change prior to v1.0.

Reference documents:

* [Bluetooth Core Specification v5.4][Core] (LE Core Configuration as defined in [Vol 0] Part B, Section 4.4)
* [Core Specification Supplement v11][CSS]
* [Assigned Numbers][AN]
* [GATT Specification Supplement][GSS]

[Core]: https://www.bluetooth.com/specifications/specs/core-specification-5-4/
[CSS]: https://www.bluetooth.com/specifications/specs/core-specification-supplement-11/
[AN]: https://www.bluetooth.com/specifications/specs/assigned-numbers/
[GSS]: https://www.bluetooth.com/specifications/specs/gatt-specification-supplement/

Profiles and services:

* [Battery Service][BAS]
* [Device Information Service][DIS]
* [HID over GATT Profile][HOGP]

[BAS]: https://www.bluetooth.com/specifications/specs/battery-service/
[DIS]: https://www.bluetooth.com/specifications/specs/device-information-service-1-1/
[HOGP]: https://www.bluetooth.com/specifications/specs/hid-over-gatt-profile-1-0/

Example Server
--------------

The [server](examples/server.rs) example brings up a demo GATT server to test controller functionality.

### Listing available Bluetooth controllers

```
$ cargo run --example server
Available controllers (pass 'ID <VID>:<PID>' to '--vid' and '--pid' options):
Bus 002 Device 012: ID 7392:c611
Bus 002 Device 003: ID 8087:0033
```

### Running the server

* Linux users need to either configure `udev` permissions or run the example binary via `sudo` to give `libusb` device write access to the USB device.
* Windows users need to follow driver installation instructions from the section below.

Look for "Burble" Bluetooth device on the client. You can use [nRF Connect for Mobile][nRF] to get more details about the server advertisements and GATT services.

Some clients may not support extended LE advertising. Use the `--legacy` option to switch to legacy advertising PDUs.

[nRF]: https://play.google.com/store/apps/details?id=no.nordicsemi.android.mcp

```
$ RUST_LOG=info cargo run --example server -- --vid 7392 --pid c611
 INFO burble::host::usb::libusb: libusb version: 1.0.26.11724
 INFO burble::host::usb::libusb: Using WinUSB backend
 INFO server: Local version: LocalVersion { hci_version: V5_1, hci_subversion: 11, lmp_version: V5_1, company_id: CompanyId(0x005D => "Realtek Semiconductor Corporation"), lmp_subversion: 34657 }
 INFO server: Device address: Public(08:BE:AC:2E:0D:EE)
 INFO burble::gatt::schema: GATT schema:
 INFO burble::gatt::schema: [0x0001] GenericAccess <0x1800>
 INFO burble::gatt::schema: [0x0002] |__ DeviceName <0x2A00>
 INFO burble::gatt::schema: [0x0003] |   |__ [Value <0x2A00>]
 INFO burble::gatt::schema: [0x0004] |__ Appearance <0x2A01>
 INFO burble::gatt::schema: [0x0005]     |__ [Value <0x2A01>]
 INFO burble::gatt::schema: [0x0006] GenericAttribute <0x1801>
 INFO burble::gatt::schema: [0x0007] |__ ServiceChanged <0x2A05>
 INFO burble::gatt::schema: [0x0008] |   |__ [Value <0x2A05>]
 INFO burble::gatt::schema: [0x0009] |   |__ ClientCharacteristicConfiguration <0x2902>
 INFO burble::gatt::schema: [0x000A] |__ ClientSupportedFeatures <0x2B29>
 INFO burble::gatt::schema: [0x000B] |   |__ [Value <0x2B29>]
 INFO burble::gatt::schema: [0x000C] |__ DatabaseHash <0x2B2A>
 INFO burble::gatt::schema: [0x000D]     |__ [Value <0x2B2A>]
 INFO burble::gatt::schema: [0x000E] (Secondary) Battery <0x180F>
 INFO burble::gatt::schema: [0x000F] |__ BatteryLevel <0x2A19>
 INFO burble::gatt::schema: [0x0010]     |__ [Value <0x2A19>]
 INFO burble::gatt::schema: [0x0011] Glucose <0x1808>
 INFO burble::gatt::schema: [0x0012] |__ [Include 0x000E..=0x0010]
 INFO burble::gatt::schema: [0x0013] |__ GlucoseMeasurement <0x2A18>
 INFO burble::gatt::schema: [0x0014]     |__ [Value <0x2A18>]
 INFO burble::gatt::schema: [0x0015]     |__ ClientCharacteristicConfiguration <0x2902>
 INFO burble::gatt::schema: [0x0016]     |__ CharacteristicExtendedProperties <0x2900>
```

Windows
-------

Either use [Zadig] to install the [WinUSB] driver for a specific Bluetooth device, or install [UsbDk], which has some known issues, but doesn't require changing device drivers. See [libusb Windows wiki page][libusb-Windows] for more info.

[Zadig]: https://zadig.akeo.ie/
[WinUSB]: https://learn.microsoft.com/en-us/windows-hardware/drivers/usbcon/winusb-installation
[UsbDk]: https://github.com/daynix/UsbDk/releases
[libusb-Windows]: https://github.com/libusb/libusb/wiki/Windows#driver-installation

### UsbDk Known Issues

#### Hanging libusb_open

If a redirected device is not closed on exit (e.g. after a crash), subsequent attempts to open it may cause the process to hang for about two minutes, followed by a "Redirector startup failed" libusb error message. See [daynix/UsbDk#105].

[daynix/UsbDk#105]: https://github.com/daynix/UsbDk/issues/105

#### WDF_VIOLATION BSOD

A `WDF_VIOLATION` [BSOD] may be caused by having multiple [multiple power policy owners] enabled for the Bluetooth USB device. There are two workarounds:

1. Disable the device in the Device Manager.
2. Uncheck the option to "Allow the computer to turn off this device to save power" in Device Properties -> Power Management tab.

[BSOD]: https://github.com/daynix/UsbDk/issues/115
[multiple power policy owners]: https://sourceforge.net/p/libusb-win32/mailman/message/25823294/

Tested Controllers
------------------

Below is a list of Bluetooth controllers that have been tested with this library.

### Server

| Device         | VID:PID   | BLE Version | Chip       | ACL Buffers |
| -------------- | --------- |:-----------:|:----------:|:-----------:|
| Edimax BT-8500 | 7392:C611 | 5.1         | RTL8761BUV | 8 * 251B    |
| Intel AX210    | 8087:0032 | 5.3         | -          | 3 * 251B    |
| Intel AX211    | 8087:0033 | 5.3         | -          | 3 * 251B    |

### Client

| Device         | VID:PID   | BLE Version | Chip       |
| -------------- | --------- |:-----------:|:----------:|
| Edimax BT-8500 | 7392:C611 | 5.1         | RTL8761BUV |
| Intel AX210    | 8087:0032 | 5.3         | -          |
| Intel AX211    | 8087:0033 | 5.3         | -          |

Legal
-----

Copyright 2023 Blackrock Neurotech. Licensed under the Mozilla Public License 2.0.

**This is not an officially supported Blackrock Neurotech product.**
