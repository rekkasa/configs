# compton --config $HOME/.config/compton.conf &
xrandr --output eDP-1 --mode 1920x1080 
# picom --experimental-backends --config $HOME/.config/picom/picom.conf &
# ~/src/picom/build/src/picom --experimental-backends --animations --animation-for-open-window zoom --animation-stiffness 400 --corner-radius 0.5 &
pcmanfm -d &
# background-loop &
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
