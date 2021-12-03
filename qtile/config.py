# Copyright (c) 2010 Aldo Cortesi
# Copyright (c) 2010, 2014 dequis
# Copyright (c) 2012 Randall Ma
# Copyright (c) 2012-2014 Tycho Andersen
# Copyright (c) 2012 Craig Barnes
# Copyright (c) 2013 horsik
# Copyright (c) 2013 Tao Sauvage
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

from typing import List  # noqa: F401
from libqtile import hook
from libqtile import bar, layout, widget
from libqtile.config import Click, Drag, Group, Key, Match, Screen
from libqtile.lazy import lazy
from libqtile.utils import guess_terminal

colors = [["#171717", "#171717"],   # 0: black
         ["#f1f1f1", "#f1f1f1"],    # 1: white
         ["#fb007a", "#fb007a"],    # 2: red
         ["#10a778", "#10a778"],    # 3: green
         ["#a89c14", "#a89c14"],    # 4: yellow
         ["#008ec4", "#008ec4"],    # 5: blue
         ["#523c79", "#523c79"],    # 6: magenta
         ["#20a5ba", "#20a5ba"]]    # 7: cyan

mod = "mod4"
terminal = guess_terminal()

keys = [
    # A list of available commands that can be bound to keys can be found
    # at https://docs.qtile.org/en/latest/manual/config/lazy.html

    # Switch between windows
    Key(
      [mod], "h", 
      lazy.layout.left(), 
      desc="Move focus to left"
    ),
    Key(
      [mod], "l", 
      lazy.layout.right(), 
      desc="Move focus to right"
    ),
    Key(
      [mod], "j", 
      lazy.layout.down(), 
      desc="Move focus down"
    ),
    Key(
      [mod], "k", 
      lazy.layout.up(), 
      desc="Move focus up"
    ),

    # Move windows between left/right columns or move up/down in current stack.
    # Moving out of range in Columns layout will create new column.
    Key(
      [mod, "shift"], "h", 
      lazy.layout.shuffle_left(),
      desc="Move window to the left"
    ),
    Key(
      [mod, "shift"], "l", 
      lazy.layout.shuffle_right(),
      desc="Move window to the right"
    ),
    Key(
      [mod, "shift"], "j", 
      lazy.layout.shuffle_down(),
      desc="Move window down"
    ),
    Key(
      [mod, "shift"], "k", 
      lazy.layout.shuffle_up(), 
      desc="Move window up"
    ),

    # Grow windows. If current window is on the edge of screen and direction
    # will be to screen edge - window would shrink.
    Key(
      [mod, "control"], "p", 
      lazy.layout.grow_left(),
      desc="Grow window to the left"
    ),
    Key(
      [mod, "control"], "l", 
      lazy.layout.grow_right(),
      desc="Grow window to the right"
    ),
    Key(
      [mod, "control"], "j", 
      lazy.layout.grow_down(),
      desc="Grow window down"
    ),
    Key(
      [mod, "control"], "k", 
      lazy.layout.grow_up(), 
      desc="Grow window up"
    ),
    Key(
      [mod], "n", 
      lazy.next_layout(), 
      desc="Toggle through layouts"
    ),

    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but still with
    # multiple stack panes
    Key(
      [mod, "shift"], "space", 
      lazy.layout.toggle_split(),
      desc="Toggle between split and unsplit sides of stack"
    ),
    Key(
      [mod, "shift"], "Return", 
      lazy.spawn(terminal), 
      desc="Launch terminal"
    ),
    Key(
      [mod], "w", 
      lazy.spawn("brave-browser"), 
      desc="Launch firefox"
    ),
    Key(
      [mod, "shift"], "b", 
      lazy.spawn("background-change"), 
      desc="Change background wallpaper"
    ),
    Key(
      [mod], "F1", 
      lazy.spawn("change-keyboard-layout"), 
      desc="Change keyboard language"
    ),

    # Toggle between different layouts as defined below
    Key(
      [mod], "Tab", 
      lazy.screen.toggle_group(),
      desc="Toggle between layouts"
    ),
    Key(
      [mod, "shift"], "c", 
      lazy.window.kill(), 
      desc="Kill focused window"
    ),

    Key(
      [mod, "control"], "r", 
      lazy.reload_config(), 
      desc="Reload the config"
    ),
    Key(
      [mod, "control"], "q", 
      lazy.shutdown(), 
      desc="Shutdown Qtile"
    ),
    Key(
      [mod], "space", 
      lazy.spawn("rofi -modi drun,run -show drun -show-icons"),
      desc="Spawn a command using a prompt widget"
    ),
    Key(
      [mod], "F2", 
      lazy.spawn("volume-control down"),
      desc="Spawn a command using a prompt widget"
    ),
    Key(
      [mod], "F3", 
      lazy.spawn("volume-control up"),
      desc="Spawn a command using a prompt widget"
    ),
    Key(
      [mod], "e", 
      lazy.spawn("emacsclient -c -a 'emacs'"),
      desc="Spawn an emacs client"
    ),
    Key(
      [mod, "shift"], "k", 
      lazy.next_screen(),
      desc="Switch to next screen"
    ),
    Key(
      [mod, "shift"], "j", 
      lazy.prev_screen(),
      desc="Switch to previous screen"
    ),
]

# groups = [Group(i) for i in "123456789"]
# groups = [Group(i) for i in "123456789"]
groups = [
    Group(name = "1", label = ""),
    Group(name = "2", label = ""),
    Group(name = "3", label = ""),
    Group(name = "4", label = ""),
    Group(name = "5", label = ""),
    Group(name = "6", label = ""),
    Group(name = "7", label = ""),
    Group(name = "8", label = "")
]


for i in groups:
    keys.extend([
        # mod1 + letter of group = switch to group
        Key([mod], i.name, lazy.group[i.name].toscreen(),
            desc="Switch to group {}".format(i.name)),

        # mod1 + shift + letter of group = switch to & move focused window to group
        Key([mod, "shift"], i.name, lazy.window.togroup(i.name, switch_group=True),
            desc="Switch to & move focused window to group {}".format(i.name)),
        # Or, use below if you prefer not to switch to that group.
        # # mod1 + shift + letter of group = move focused window to group
        # Key([mod, "shift"], i.name, lazy.window.togroup(i.name),
        #     desc="move focused window to group {}".format(i.name)),
    ])

layout_basic={
        "border.width": 8,
        "margin": 1,
        "border_focus": colors[6],
        "border_normal": colors[0]
        }

layouts = [
    layout.MonadTall(**layout_basic),
    layout.Max(**layout_basic),
]

widget_defaults = dict(
    font='sans',
    fontsize=12,
    padding=3,
)
extension_defaults = widget_defaults.copy()

# ----------------------------------
# Need to turn this into a function
# ----------------------------------
screens = [
    Screen(
        top=bar.Bar(
            [
                widget.GroupBox(
                    font="FontAwesome",
                    fontsize=14,
                    rounded=False,
                    active=colors[2],
                    inactive=colors[3],
                    highlight_method="block",
                    this_current_screen_border=colors[1]
                ),
                widget.Prompt(),
                widget.WindowName(
                    font="DejaVu Sans Mono Bold",
                    fontsize=14
                ),
                widget.CPU(
                    font="DejaVu Sans Mono Bold",
                    fontsize=14,
                    foreground=colors[2],
                    format='CPU: {load_percent}%'
                ),
                widget.Sep(
                    linewidth=0,
                    padding=10
                ),
                widget.Memory(
                    font="DejaVu Sans Mono Bold",
                    fontsize=14,
                    foreground=colors[5],
                    format='RAM: {MemPercent}% [{MemUsed: .2f} {mm} ]',
                    measure_mem='G'
                ),
                widget.Sep(
                    linewidth=0,
                    padding=8
                ),
                widget.Battery(
                    font="DejaVu Sans Mono Bold",
                    fontsize=14,
                    foreground=colors[4],
                    format='Battery: {percent:2.0%} [ {char} ]',
                    discharge_char="discharing",
                    charge_char = "charging",
                    update_interval = 60,
                    hide_threshold = 0.6
                ),
                # widget.ThermalSensor(),
                widget.Clock(
                    font="DejaVu Sans Mono Bold",
                    fontsize=14,
                    foreground=colors[3],
                    format='%Y-%m-%d %a %I:%M %p'
                ),
            ],
            24,
            background=colors[0],
            # border_width=[2, 0, 2, 0],  # Draw top and bottom borders
            # border_color=["ff00ff", "000000", "ff00ff", "000000"]  # Borders are magenta
        ),
    ),
    Screen(
        top=bar.Bar(
            [
                widget.GroupBox(
                    font="FontAwesome",
                    fontsize=14,
                    rounded=False,
                    active=colors[2],
                    inactive=colors[3],
                    highlight_method="block",
                    this_current_screen_border=colors[1]
                ),
                widget.Prompt(),
                widget.WindowName(
                    font="DejaVu Sans Mono Bold",
                    fontsize=14
                ),
                widget.CPU(
                    font="DejaVu Sans Mono Bold",
                    fontsize=14,
                    foreground=colors[2],
                    format='CPU: {load_percent}%'
                ),
                widget.Sep(
                    linewidth=0,
                    padding=10
                ),
                widget.Memory(
                    font="DejaVu Sans Mono Bold",
                    fontsize=14,
                    foreground=colors[5],
                    format='RAM: {MemPercent}% [{MemUsed: .2f} {mm} ]',
                    measure_mem='G'
                ),
                widget.Sep(
                    linewidth=0,
                    padding=8
                ),
                widget.Battery(
                    font="DejaVu Sans Mono Bold",
                    fontsize=14,
                    foreground=colors[4],
                    format='Battery: {percent:2.0%} [ {char} ]',
                    discharge_char="discharing",
                    charge_char = "charging",
                    update_interval = 60,
                    hide_threshold = 0.6
                ),
                # widget.ThermalSensor(),
                widget.Clock(
                    font="DejaVu Sans Mono Bold",
                    fontsize=14,
                    foreground=colors[3],
                    format='%Y-%m-%d %a %I:%M %p'
                ),
            ],
            24,
            background=colors[0],
            # border_width=[2, 0, 2, 0],  # Draw top and bottom borders
            # border_color=["ff00ff", "000000", "ff00ff", "000000"]  # Borders are magenta
        ),
    ),
    Screen(
        top=bar.Bar(
            [
                widget.GroupBox(
                    font="FontAwesome",
                    fontsize=14,
                    rounded=False,
                    active=colors[2],
                    inactive=colors[3],
                    highlight_method="block",
                    this_current_screen_border=colors[1]
                ),
                widget.Prompt(),
                widget.WindowName(
                    font="DejaVu Sans Mono Bold",
                    fontsize=14
                ),
                widget.CPU(
                    font="DejaVu Sans Mono Bold",
                    fontsize=14,
                    foreground=colors[2],
                    format='CPU: {load_percent}%'
                ),
                widget.Sep(
                    linewidth=0,
                    padding=10
                ),
                widget.Memory(
                    font="DejaVu Sans Mono Bold",
                    fontsize=14,
                    foreground=colors[5],
                    format='RAM: {MemPercent}% [{MemUsed: .2f} {mm} ]',
                    measure_mem='G'
                ),
                widget.Sep(
                    linewidth=0,
                    padding=8
                ),
                widget.Battery(
                    font="DejaVu Sans Mono Bold",
                    fontsize=14,
                    foreground=colors[4],
                    format='Battery: {percent:2.0%} [ {char} ]',
                    discharge_char="discharing",
                    charge_char = "charging",
                    update_interval = 60,
                    hide_threshold = 0.6
                ),
                # widget.ThermalSensor(),
                widget.Clock(
                    font="DejaVu Sans Mono Bold",
                    fontsize=14,
                    foreground=colors[3],
                    format='%Y-%m-%d %a %I:%M %p'
                ),
            ],
            24,
            background=colors[0],
            # border_width=[2, 0, 2, 0],  # Draw top and bottom borders
            # border_color=["ff00ff", "000000", "ff00ff", "000000"]  # Borders are magenta
        ),
    ),
]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front())
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: List
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating(float_rules=[
    # Run the utility of `xprop` to see the wm class and name of an X client.
    *layout.Floating.default_float_rules,
    Match(wm_class='confirmreset'),  # gitk
    Match(wm_class='makebranch'),  # gitk
    Match(wm_class='maketag'),  # gitk
    Match(wm_class='ssh-askpass'),  # ssh-askpass
    Match(title='branchdialog'),  # gitk
    Match(title='pinentry'),  # GPG key password entry
])
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True

@hook.subscribe.client_new
def move_program_to_group(window):
    window_groups = {
        "Mail": '1',
        "microsoft teams - preview": '2',
        "keepassxc": '3',
        "emacs": '5',
        "qpdfview": '6',
        "brave-browser": '8'
    }
    wm_class = window.window.get_wm_class()[0] 
    window.togroup(window_groups[wm_class])

wmname = "LG3D"
