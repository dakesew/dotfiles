echo "Uses yaourt"
yaourt -S --needed xorg xorg-xinit xbindkeys lxdm \
i3-gaps-git i3status i3lock python2 python2-imaging xpyb adobe-source-code-pro-fonts rofi feh \
firefox engrampa thunar transmission-gtk thunar-volman thunar-shares-plugin \
zsh zsh-completions zsh-syntax-highlighting grml-zsh-config \
gcc git make mlocate gdb emacs \
avr-gcc avr-gdb avrdude \
arm-none-eabi-gcc openocd arm-none-eabi-gdb 
sudo ln -s ~/.i3/emc /bin/emc
