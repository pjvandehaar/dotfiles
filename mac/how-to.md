- **Volume:** _shift-option-volumekey_
- **Files belonging to packages**: `pkgutil --pkgs; pkgutil --files <pkgname>`
- **check MAC address**:
    - `ifconfig en0 | grep ether # some macs: en1`
    - `networksetup -listallhardwareports`
    - option-click on wifi in menubar (probably unspoofed builtin MAC)
- **spoof MAC address**:
    1. dissociate: `sudo airport --disassociate` (but leave wifi on)
    2. `sudo ifconfig en0 ether aa:bb:cc:dd:ee:ff # or perhaps "en1" and/or "Wi-Fi"`
        - `sudo ifconfig en0 ether $(python3 -c "print('%x'%(0x$(ifconfig en0 ether|grep ether|ptrcut 2|tr -d :)+1))" | perl -nle '@f= $_=~/../g; print join ":",@f')`
    3. if receiving `bad value`, try `sudo ifconfig en0 down && sudo ifconfig en0 up`
    4. MAC should return to hardware default on reboot
