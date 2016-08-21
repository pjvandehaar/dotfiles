- **Volume:** _shift-option-volumekey_
- **Files belonging to packages**: `pkgutil --pkgs; pkgutil --files <pkgname>`
- **check MAC address**:
    - `ifconfig en0 | grep ether # some macs: en1`
    - `networksetup -listallhardwareports`
    - option-click on wifi in menubar (probably unspoofed builtin MAC)
- **spoof MAC address**:
    1. dissociate: `airport --disassociate`
    2. `sudo ifconfig en0 ether aa:bb:cc:dd:ee:ff # or perhaps "en1" and/or "Wi-Fi"`
    3. if receiving `bad value`, try `sudo ifconfig en0 down && sudo ifconfig en0 up`
    4. MAC should return to hardware default on reboot
