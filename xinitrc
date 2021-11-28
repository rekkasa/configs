# compton --config $HOME/.config/compton.conf &
xrandr --output eDP-1 --mode 1920x1080 
# picom --experimental-backends --config $HOME/.config/picom/picom.conf &
pcmanfm -d &
background-loop &
feh --bg-scale $HOME/wallpaper &
flameshot &
xrandr --auto --output HDMI-1 --right-of DP-2 &
emacs --daemon &
# pulseaudio --start
dunst &
# syncthing &
# exec slstatus &
# exec dwm
qtile start
