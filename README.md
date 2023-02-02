Blackrock User-Mode Bluetooth LE Library
========================================

A user-mode BLE stack implementation starting from the USB transport layer via [libusb]. Tested on Windows and Linux.

**Status:** Under development

Reference documents:

* [Bluetooth Core Specification v5.3][core] (LE Core Configuration as defined in [Vol 0] Part B, Section 4.4)
* [Core Specification Supplement v10][css]
* [Assigned Numbers][an]

[libusb]: https://github.com/libusb/libusb
[core]: https://www.bluetooth.com/specifications/specs/core-specification-5-3/
[css]: https://www.bluetooth.com/specifications/specs/core-specification-supplement-10/
[an]: https://www.bluetooth.com/specifications/assigned-numbers/

Example Server
--------------

The [server](examples/server.rs) example brings up a demo GATT server to test controller functionality. Currently, the server can only be tested by a client application that does not require pairing, such as [nRF Connect for Mobile][nRF].

[nRF]: https://play.google.com/store/apps/details?id=no.nordicsemi.android.mcp

### Listing available Bluetooth controllers

```
$ cargo run --example server
Available controllers (pass 'ID <VID>:<PID>' to '--vid' and '--pid' options):
Bus 002 Device 012: ID 7392:c611
Bus 002 Device 003: ID 8087:0033
```

### Running the server

* Linux users need to either configure `udev` permissions or run the example binary via `sudo` to give `libusb` device write access.
* Windows users need to follow driver installation instructions from the section below.

Look for "Burble" device on the client.

```
$ RUST_LOG=debug cargo run --example server -- --vid 7392 --pid c611
 INFO burble::host::usb::libusb: libusb version: 1.0.26.11724
DEBUG burble::host::usb::libusb: - LIBUSB_CAP_HAS_CAPABILITY = true
DEBUG burble::host::usb::libusb: - LIBUSB_CAP_HAS_HOTPLUG = false
DEBUG burble::host::usb::libusb: - LIBUSB_CAP_SUPPORTS_DETACH_KERNEL_DRIVER = false
 INFO burble::host::usb::libusb: Using WinUSB backend
DEBUG burble::host::usb: Opening ID 7392:C611
DEBUG burble::host::usb: Event thread started
DEBUG burble::host::usb::libusb: [cache_config_descriptors] could not access configuration descriptor 1 (dummy) for 'USB\VID_0BDA&PID_8153\000001000000': [31] A device attached to the system is not functioning.
DEBUG burble::host::usb: Bluetooth device at Bus 002 Device 012: ID 7392:c611
DEBUG burble::host::usb: Claiming main interface
DEBUG burble::host::usb: Claiming isochronous interface
DEBUG burble::host::usb: Setting isochronous interface alt setting to 0
DEBUG burble::hci::event: Event receiver task started
 INFO server: Local version: LocalVersion { hci_version: V5_1, hci_subversion: 11, lmp_version: V5_1, company_id: CompanyId(0x005D => "Realtek Semiconductor Corporation"), lmp_subversion: 34657 }
 INFO server: Device address: Public(08:BE:AC:2E:0D:EE)
DEBUG burble::l2cap: Controller buffers: LeBufferSize { acl_data_len: 251, acl_num_pkts: 8, iso_data_len: 0, iso_num_pkts: 0 }
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
