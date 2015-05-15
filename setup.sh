echo "Starting now"
echo "Doing symlinks"
ln -s $HOME/dotfiles/emacs/.spacemacs     $HOME/.spacemacs
ln -s $HOME/dotfiles/emacs/.emacs.d       $HOME/.emacs.d
ln -s $HOME/dotfiles/i3/.i3               $HOME/.i3
ln -s $HOME/dotfiles/firefox/.mozilla     $HOME/.mozilla
ln -s $HOME/dotfiles/firefox/.vimperator  $HOME/.vimperator
echo "Finished symlinks"
echo "Mapping Caps Lock to Escape (for vi-esque editors)"
xmodmap -e 'clear Lock' -e 'keycode 0x42 = Escape'
echo "Remember to set Firefox taband starting page, and to set vimperator colosheme"
echo "Duckduckgo URL: https://duckduckgo.com/?kae=d&kak=-1&kal=-1&kj=383838&k7=282828&k9=d8d8d8&kaa=d8d8d8&k8=b8b8b8&kx=888888&ky=383838&ko=1"
