Blackrock User-Mode Bluetooth LE Library
========================================

[![crates.io](https://img.shields.io/crates/v/burble?style=for-the-badge)](https://crates.io/crates/burble)
[![docs.rs](https://img.shields.io/badge/docs.rs-burble-66c2a5?style=for-the-badge&logo=docs.rs)](https://docs.rs/burble)
[![License](https://img.shields.io/crates/l/burble?style=for-the-badge)](https://choosealicense.com/licenses/mpl-2.0/)

A cross-platform user-mode BLE stack implementation starting from the USB transport layer via [libusb].

[libusb]: https://github.com/libusb/libusb

**Project status:** Under active development. Tested on Linux, macOS, and Windows. Not accepting external contributions at this time. The library implements all components for a BLE GATT server (peripheral role) and LE Secure Connections pairing. Minimum supported Bluetooth version is 5.0. All APIs are subject to change prior to v1.0.

Burble exposes the full functionality of a BLE controller, so some familiarity with the Bluetooth Core Specification and other relevant documents is expected.

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

Getting Started
---------------

The [server] example brings up a demo GATT server to test controller functionality. See OS-specific sections below for instructions on allowing `libusb` to access the controllers from user-space.

[server]: https://github.com/BlackrockNeurotech/burble/blob/main/examples/server.rs

### Listing available Bluetooth controllers

```text
$ cargo run --example server
Available controllers (pass 'ID <VID>:<PID>' to '--vid' and '--pid' options):
Bus 002 Device 012: ID 7392:c611
Bus 002 Device 003: ID 8087:0033
```

### Running the server

Start the server and look for "Burble" Bluetooth device on the client. You can use [nRF Connect for Mobile][nRF] to get more details about the server advertisements and GATT services.

Some clients may not support extended LE advertising. Use the `--legacy` option to switch to legacy advertising PDUs.

[nRF]: https://play.google.com/store/apps/details?id=no.nordicsemi.android.mcp

```text
$ RUST_LOG=debug cargo run --example server -- --vid 7392 --pid c611
 INFO burble::host::usb::libusb: libusb version: 1.0.26.11724
DEBUG burble::host::usb::libusb: - LIBUSB_CAP_HAS_CAPABILITY = true
DEBUG burble::host::usb::libusb: - LIBUSB_CAP_HAS_HOTPLUG = false
DEBUG burble::host::usb::libusb: - LIBUSB_CAP_SUPPORTS_DETACH_KERNEL_DRIVER = false
 INFO burble::host::usb::libusb: Using WinUSB backend
 INFO burble::host::usb: Opening device ID 7392:C611
DEBUG burble::host::usb: Event thread started
 INFO burble::host::usb::libusb: Resetting Bus 002 Device 012: ID 7392:c611
DEBUG burble::host::usb: Bluetooth device at Bus 002 Device 012: ID 7392:c611
DEBUG burble::host::usb: Claiming main interface
DEBUG burble::host::usb: Claiming isochronous interface
DEBUG burble::host::usb: Setting isochronous interface alt setting to 0
DEBUG burble::hci: Event loop started
DEBUG burble::hci: HCI reset...
DEBUG burble::hci: Controller version: LocalVersion { hci_version: v5.1, hci_subversion: 11, lmp_version: v5.1, company_id: CompanyId(0x005D => "Realtek Semiconductor Corporation"), lmp_subversion: 34657 }
DEBUG burble::hci: Controller LMP features: LmpFeature(LE_SUPPORTED | EXTENDED_FEATURES | 0x77bfd9bfeffffff)
DEBUG burble::hci: Controller LE features: LeFeature(ENCRYPTION | EXTENDED_REJECT_INDICATION | PERIPHERAL_INITIATED_FEATURES_EXCHANGE | PING | DATA_PACKET_LENGTH_EXTENSION | LL_PRIVACY | EXTENDED_SCANNING_FILTER_POLICIES | LE_2M_PHY | STABLE_MODULATION_INDEX_TRANSMITTER | STABLE_MODULATION_INDEX_RECEIVER | LE_CODED_PHY | EXTENDED_ADVERTISING | CHANNEL_SELECTION_ALGORITHM_2 | CONNECTION_CTE_REQUEST | CONNECTION_CTE_RESPONSE | ANTENNA_SWITCHING_DURING_CTE_TRANSMISSION | ANTENNA_SWITCHING_DURING_CTE_RECEPTION)
DEBUG burble::hci: Controller LE states: 0b111111111111111111111111111111111111111111
DEBUG burble::hci: Controller LE buffers: LeBufferSize { acl_data_len: 251, acl_num_pkts: 8, iso_data_len: 0, iso_num_pkts: 0 }
DEBUG burble::hci: Controller address: Public(08:BE:AC:2E:0D:EE)
DEBUG burble::gatt::db: GATT database:
DEBUG burble::gatt::db: [0x0001] Service(GenericAttribute) <0x1801>
DEBUG burble::gatt::db: [0x0002] |__ Characteristic(ServiceChanged) <0x2A05>
DEBUG burble::gatt::db: [0x0003] |   |__ [Value <0x2A05>]
DEBUG burble::gatt::db: [0x0004] |   |__ Descriptor(ClientCharacteristicConfiguration) <0x2902>
...
 INFO server: Enabling advertisements
```

Linux
-----

Burble requires read/write access to the USB device node, which is normally restricted to root. On systems with [udev], the following rules file can be used to provide access to the logged-in user (adjust the example as needed to restrict access, and set `vendor_id` and `product_id`):

```text
/etc/udev/rules.d/99-burble.rules:

SUBSYSTEMS=="usb", ATTRS{idVendor}=="vendor_id", ATTRS{idProduct}=="product_id", MODE="0660", TAG+="uaccess"
```

Use `sudo udevadm control --reload-rules && sudo udevadm trigger` to apply the permissions.

[udev]: https://wiki.archlinux.org/title/udev

Windows
-------

Either use [Zadig] to install the [libusbK] driver for a specific Bluetooth device (recommended), or install [UsbDk], which has some known issues, but doesn't require changing device drivers. See [libusb Windows wiki page][libusb-Windows] for more info.

[Zadig]: https://zadig.akeo.ie/
[libusbK]: https://github.com/mcuee/libusbk
[UsbDk]: https://github.com/daynix/UsbDk/releases
[libusb-Windows]: https://github.com/libusb/libusb/wiki/Windows#driver-installation

### Using Zadig

1. Run Zadig and enable Options → List All Devices.
2. Select the target controller.
3. Install either [libusbK] or [WinUSB] driver. The former is recommended because it can reset the USB device. Each driver has some known issues, so if you're having problems with one, try the other.

[WinUSB]: https://learn.microsoft.com/en-us/windows-hardware/drivers/usbcon/winusb-installation

### UsbDk Known Issues

Unfortunately, UsbDk appears to be unmaintained, so its use is discouraged.

#### Hanging libusb_open

If a redirected device is not closed on exit (e.g. after a crash), subsequent attempts to open it may cause the process to hang for about two minutes, followed by a "Redirector startup failed" libusb error message. See [daynix/UsbDk#105].

[daynix/UsbDk#105]: https://github.com/daynix/UsbDk/issues/105

#### WDF_VIOLATION BSOD

A `WDF_VIOLATION` [BSOD] may be caused by having multiple [multiple power policy owners] enabled for the Bluetooth USB device. There are two workarounds:

1. Disable the device in the Device Manager.
2. Uncheck the option to "Allow the computer to turn off this device to save power" in Device Properties -> Power Management tab.

[BSOD]: https://github.com/daynix/UsbDk/issues/115
[multiple power policy owners]: https://sourceforge.net/p/libusb-win32/mailman/message/25823294/

FAQ
---

### What are the goals of this project?

Burble aims to become a feature-complete Bluetooth LE library, implementing HCI, L2CAP, GAP, ATT, GATT, and SMP layers for both the Central and Peripheral roles.

### How is this different from other Bluetooth libraries?

Most libraries use OS-specific APIs and drivers to access the controller. Burble communicates with the controller directly over USB (or another transport), bypassing all OS-specific functionality.

### What are the downsides to this approach?

Burble requires exclusive access to the controller. The OS and other applications cannot use the controller at the same time. On Windows, this means installing a libusb-compatible driver which prevents the OS from identifying the controller as a Bluetooth device. On Linux and macOS, the driver is automatically detached while Burble is using the controller.

Another potential downside is loss of vendor-specific functionality. Though this can be added for individual controllers, Burble focuses on implementing the Core Bluetooth Specification that is common to all controllers.

### What are the advantages?

Burble supports all major operating systems and can take advantage of the features introduced in the most recent versions of the Bluetooth Core Specification (subject to controller support). Having exclusive controller access allows complete control over all aspects of the controller operation, such as advertising, scanning, and GATT services. This is particularly useful for implementing the peripheral role when you need specific configuration for GAP and GATT services.

### What is Burble's approach to security?

Burble places heavy emphasis on security and correctness. Legacy features that can lead to insecure operation are simply not implemented (e.g. LE legacy pairing, < 128-bit encryption keys). All cryptographic primitives used by the Security Manager are implemented in a [separate package][crypto] that forbids unsafe code to facilitate auditing.

[crypto]: https://github.com/BlackrockNeurotech/burble/tree/main/crypto

### Can Burble be used in embedded (`no_std`) systems?

Currently, no, but this is an eventual goal. A few components, like the libusb event thread, currently require `std`. These will be put behind feature flags or redesigned to allow Burble core to function on any system that can implement the `host::Transport` trait.

Tested Controllers
------------------

Below is a list of Bluetooth controllers that have been tested with this library.

### Server

| Device         | VID:PID   | BLE Version |    Chip    | ACL Buffers |
|----------------|-----------|:-----------:|:----------:|:-----------:|
| Edimax BT-8500 | 7392:C611 |     5.1     | RTL8761BUV |  8 * 251B   |
| Intel AX210    | 8087:0032 |     5.3     |     -      |  3 * 251B   |
| Intel AX211    | 8087:0033 |     5.3     |     -      |  3 * 251B   |

### Client

| Device         | VID:PID   | BLE Version |    Chip    |
|----------------|-----------|:-----------:|:----------:|
| Edimax BT-8500 | 7392:C611 |     5.1     | RTL8761BUV |
| Intel AX210    | 8087:0032 |     5.3     |     -      |
| Intel AX211    | 8087:0033 |     5.3     |     -      |

Legal
-----

Copyright 2023 Blackrock Neurotech. Licensed under the Mozilla Public License 2.0.

**This is not an officially supported Blackrock Neurotech product.**
