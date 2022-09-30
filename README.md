Blackrock User-Mode Bluetooth LE Library
========================================

A user-mode BLE stack implementation starting from USB transport layer via [libusb]. Tested on Windows and Linux.

[libusb]: https://github.com/libusb/libusb

Windows
-------

Install [usbdk] driver. See [libusb Windows wiki page][libusb-Windows] for more info.

[usbdk]: https://github.com/daynix/UsbDk/releases
[libusb-Windows]: https://github.com/libusb/libusb/wiki/Windows#driver-installation

### WDF_VIOLATION BSOD

A `WDF_VIOLATION` BSOD may be caused by having multiple [multiple power policy owners] enabled for the Bluetooth USB device. This is a [bug in usbdk]. There are two workarounds:

1. Disable the device in the Device Manager.
2. Uncheck the option to "Allow the computer to turn off this device to save power" in Device Properties -> Power Management tab.

[multiple power policy owners]: https://sourceforge.net/p/libusb-win32/mailman/message/25823294/
[bug in usbdk]: https://github.com/daynix/UsbDk/issues/115
