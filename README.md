Blackrock User-Mode Bluetooth LE Library
========================================

A user-mode BLE stack implementation starting from the USB transport layer via [libusb]. Based on [Bluetooth Core Specification v5.3][spec]. Tested on Windows and Linux.

**Status:** Under development

[libusb]: https://github.com/libusb/libusb
[spec]: https://www.bluetooth.com/specifications/specs/core-specification-5-3/

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
