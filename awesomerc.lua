--=================================================================================
--=========================== Load libraries ======================================
--=================================================================================
-- Standard awesome library
awful = require("awful")
awful.autofocus = require("awful.autofocus")
awful.rules = require("awful.rules")
-- Theme handling library
beautiful = require("beautiful")
-- Notification library
naughty = require("naughty")
-- Widget library
vicious = require("vicious")

-- Load Debian menu entries
-- debian = require("debian")
debianmenu = require("debian.menu")

-- import widgets
wibox = require("wibox")

--=================================================================================
--=========================== Error Handling ======================================
--=================================================================================

-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
   naughty.notify({ preset = naughty.config.presets.critical,
                    title = "Oops, there were errors during startup!",
                    text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
   local in_error = false
   awesome.connect_signal("debug::error", function (err)
                             -- Make sure we don't go into an endless error loop
                             if in_error then return end
                             in_error = true

                             naughty.notify({ preset = naughty.config.presets.critical,
                                              title = "Oops, an error happened!",
                                              text = err })
                             in_error = false
   end)
end

-- use naughty to print the given table's members to the screen
function notify_table(tab)
   foo=string.format("type of tab is: %s\n", type(tab))
   for k,v in pairs(tab) do
      foo = foo .. string.format(" %s -> %s\n", tostring(k), tostring(v))
   end
   naughty.notify({title=string.format(foo), text=""})
end

--=================================================================================
--========================= Variable Definitions ==================================
--=================================================================================

-- Themes define colours, icons, and wallpapers
beautiful.init(awful.util.getdir("config") .. "/themes/awesome-solarized/dark/theme.lua")

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
layouts = {
   awful.layout.suit.tile,
   awful.layout.suit.tile.left,
   awful.layout.suit.tile.bottom,
   awful.layout.suit.tile.top,
   awful.layout.suit.fair,
   awful.layout.suit.fair.horizontal,
   awful.layout.suit.max,
   awful.layout.suit.max.fullscreen,
   awful.layout.suit.floating
}

-- get hostname
hostname = awful.util.pread("hostname"):gsub("\n", "")
havebattery = hostname == "hermes" or hostname == "pazuzu"

--=================================================================================
--=================================== Tags ========================================
--=================================================================================

-- Define a tag table which hold all screen tags.
tags = {}
if hostname == "lanning" then
   tags = {
      {
         names  = { "ff", "term", "ec", "4", "5", "6", "7", "8", "rss" },
         layout = { layouts[1], layouts[1], layouts[1], layouts[1], layouts[1],
                    layouts[1], layouts[1], layouts[1], layouts[1], layouts[1] }
      },
      {
         names  = { "ff", "term", "ec", "4", "5", "6", "7", "mus", "vid" },
         layout = { layouts[1], layouts[1], layouts[1], layouts[1], layouts[1],
                    layouts[1], layouts[1], layouts[1], layouts[1], layouts[7] }
      }
   }
elseif hostname == "vulcan" then
   tags = {
      {
         names  = { "ff", "term", "ec", "4", "5", "6", "7", "vm", "music" },
         layout = { layouts[1], layouts[1], layouts[1], layouts[1], layouts[1],
                    layouts[1], layouts[1], layouts[1], layouts[7], layouts[1] }
      },
      {
         names  = { "ff", "term", "ec", "4", "5", "6", "kp", "stat", "vid" },
         layout = { layouts[1], layouts[1], layouts[1], layouts[1], layouts[1],
                    layouts[1], layouts[1], layouts[1], layouts[7], layouts[7] }
      }
   }
elseif hostname == "hermes" or hostname == "pazuzu" then
   tags = {
      {
         names  = { "ff1", "term", "ec", "4", "5", "kp", "im", "em", "ff2" },
         layout = { layouts[1], layouts[1], layouts[1], layouts[1], layouts[1],
                    layouts[1], layouts[1], layouts[1], layouts[1], layouts[1] }
      }
   }
end

for s = 1, screen.count() do
   if (s <= #tags) then
      tags[s] = awful.tag(tags[s].names, s, tags[s].layout)
   else
      tagnames = {}
      taglayouts = {}
      for i = 1, 9 do
         tagnames[i] = tostring(i)
         taglayouts[i] = layouts[1]
      end
      tags[s] = awful.tag(tagnames, s, taglayouts)
   end
end

--=================================================================================
--=================================== Menu ========================================
--=================================================================================

-- Create a laucher widget and a main menu
myawesomemenu = {
   { "restart", awesome.restart },
   { "quit", awesome.quit }
}

mymainmenu = awful.menu({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
                                    { "Debian", debianmenu.Debian_menu.Debian },
}
                       })


--=================================================================================
--==================================== Panel ======================================
--=================================================================================


----------------
-- separators --
----------------
separators = {}
for c = 1, 10 do
   separators[c] = wibox.widget.textbox()
   separators[c]:set_text(" | ")
end
spacers = {}
for c = 1, 10 do
   spacers[c] = wibox.widget.textbox()
   spacers[c]:set_text(" ")
end

--------------
-- launcher --
--------------
mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
                                     menu = mymainmenu })

-------------------
-- volume widget --
-------------------

volumewidget = awful.widget.progressbar()
volumewidget:set_width(8)
volumewidget:set_vertical(true)
volumewidget:set_background_color("#000000")
volumewidget:set_border_color(nil)
volumewidget:set_color("#999999")
vicious.register(volumewidget, vicious.widgets.volume, "$1", 1, "Master")

-- -------------------
-- -- expose button --
-- -------------------
-- -- exposebutton = widget({type = "textbox"})
-- -- exposebutton:set_text(" expose ")
-- exposebutton = awful.widget.button({ image = image(beautiful.awesome_icon)})
-- exposebutton:buttons(awful.util.table.join(
--                         awful.button({ }, 1, revelation)
--                         -- awful.button({ }, 3, function () awful.util.spawn("zenity --notification --text=\"foo\"") end)
--                                           ))
-------------------
-- battery usage --
-------------------

-- only have to set this up when we have a battery
batterybox = wibox.layout.fixed.horizontal()
if havebattery then
   -- spacer and separator
   sep = wibox.widget.textbox()
   sep:set_text(" | ")
   spa = wibox.widget.textbox()
   spa:set_text("  ")

   -- progress bar display
   chargebar = awful.widget.progressbar()
   chargebar:set_width(50)
   -- chargebar:set_height(10)
   chargebar:set_background_color("#000000")
   chargebar:set_border_color(nil)
   chargebar:set_color("#eeee00")
   vicious.register(chargebar, vicious.widgets.bat, "$2", 1, "BAT1")

   -- text - time to full/empty charge
   chargetext = wibox.widget.textbox()
   vicious.register(chargetext, vicious.widgets.bat, "$3 $1", 1, "BAT1")

   -- build layout
   batterybox:add(chargebar)
   batterybox:add(spa)
   batterybox:add(chargetext)
   batterybox:add(sep)
end

------------------
-- memory usage --
------------------

-- init
memwidget = awful.widget.graph()
-- config
memwidget:set_width(60)
memwidget:set_background_color("#000000")
memwidget:set_color("#00ee00")
-- register
vicious.register(memwidget, vicious.widgets.mem, "$1", 1)

---------------
-- CPU usage --
---------------

ncpus = tonumber(io.popen("cat /proc/cpuinfo | grep '^processor' | wc -l"):read('*all'))

vicious.cache(vicious.widgets.cpu)
cpubox = wibox.layout.fixed.horizontal()
for c = 1, ncpus do
   -- separator
   local cpuspacer = wibox.widget.textbox()
   cpuspacer:set_text(" ")
   cpubox:add(cpuspacer)
   -- widget
   local cpuwidget = awful.widget.graph()
   cpuwidget:set_width(60)
   cpuwidget:set_background_color("#000000")
   cpuwidget:set_color("#0000cc")
   vicious.register(cpuwidget, vicious.widgets.cpu, string.format("$%d", c+1), 2)
   cpubox:add(cpuwidget)
end

-----------
-- clock --
-----------
mytextclock = awful.widget.textclock()

-----------------
-- system tray --
-----------------
mysystray = wibox.widget.systray()

-----------------------------
-- create wibox and set up --
-----------------------------
mywibox = {}
mylayoutbox = {}
mytaglist = {}
mytasklist = {}

-- what each mouse button does on the tag list
mytaglist.buttons = awful.util.table.join(
   awful.button({ }, 1, awful.tag.viewonly), -- click: view only this tag
   awful.button({ modkey }, 1, awful.client.movetotag), -- mod-click: move client to tag
   awful.button({ }, 3, awful.tag.viewtoggle),          -- right: view multiple tags
   awful.button({ modkey }, 3, awful.client.toggletag), -- mod-right: put client on multiple tags
   awful.button({ }, 4, awful.tag.viewprev),            -- wheel up: prev tag
   awful.button({ }, 5, awful.tag.viewnext)             -- wheel down: next tag
)

-- what each button does on the window list
mytasklist.buttons = awful.util.table.join(
   awful.button({ }, 1, function (c)
                   -- if focused, minimize
                   if c == client.focus then
                      c.minimized = true
                   else
                      -- if not focused, make visible and focus
                      if not c:isvisible() then
                         awful.tag.viewonly(c:tags()[1])
                      end
                      -- This will also un-minimize
                      -- the client, if needed
                      client.focus = c
                      c:raise()
                   end
   end),
   awful.button({ }, 3, function ()
                   if instance then
                      instance:hide()
                      instance = nil
                   else
                      instance = awful.menu.clients({ width=250 })
                   end
   end),
   -- scroll buttons scroll
   awful.button({ }, 4, function ()
                   awful.client.focus.byidx(-1)
                   if client.focus then client.focus:raise() end
   end),
   awful.button({ }, 5, function ()
                   awful.client.focus.byidx(1)
                   if client.focus then client.focus:raise() end
end))

for s = 1, screen.count() do
   
   -- Widget that will indicate which layout we're using and let us
   -- switch it around with the mouse
   mylayoutbox[s] = awful.widget.layoutbox(s)
   mylayoutbox[s]:buttons(awful.util.table.join(
                             awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
                             awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
                             awful.button({ }, 4, function () awful.layout.inc(layouts, 1) end),
                             awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)))

   -- Create a taglist widget
   mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.filter.all, mytaglist.buttons)

   -- Create a tasklist widget
   mytasklist[s] = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, mytasklist.buttons)


   -- create a left layout for the launcher and the tags list
   local left_layout = wibox.layout.fixed.horizontal()
   left_layout:add(mylauncher)
   left_layout:add(mytaglist[s])
   left_layout:add(separators[3])
   -- create a right layout for graphs, systray, clock, and layout indicator
   local right_layout = wibox.layout.fixed.horizontal()
   if s == 1 and havebattery then right_layout:add(batterybox) end
   if s == 1 then right_layout:add(memwidget) end
   if s == 1 then right_layout:add(cpubox) end
   if s == 1 then right_layout:add(separators[5]) end
   if s == 1 then right_layout:add(volumewidget) end
   right_layout:add(separators[4])
   if s == 1 then right_layout:add(mysystray) end
   if s == 1 then right_layout:add(mytextclock) end
   right_layout:add(mylayoutbox[s])

   -- put them all together with the tasklist filling it all in
   local top_layout = wibox.layout.align.horizontal()
   top_layout:set_first(left_layout)
   top_layout:set_second(mytasklist[s])
   top_layout:set_third(right_layout)

   -- -- Create the wibox
   mywibox[s] = awful.wibox({ position = "top", height = "20", screen = s })
   -- -- Add widgets to the wibox - order matters
   mywibox[s]:set_widget(top_layout)
end

--=================================================================================
--==================================== Mouse ======================================
--=================================================================================

-- root.buttons(awful.util.table.join(
--                 awful.button({ }, 3, function () mymainmenu:toggle() end),
--                 awful.button({ }, 4, awful.tag.viewnext),
--                 awful.button({ }, 5, awful.tag.viewprev)
--                                   ))

--=================================================================================
--================================= Keyboard ======================================
--=================================================================================

globalkeys = awful.util.table.join(
   -- personal shortcuts
   awful.key({ }, "XF86AudioRaiseVolume",     function() awful.util.spawn_with_shell("amixer set Master 9%+") end),
   awful.key({ }, "XF86AudioLowerVolume",     function() awful.util.spawn_with_shell("amixer set Master 9%-") end),
   awful.key({ }, "XF86AudioMute",            function() awful.util.spawn_with_shell("amixer set Master toggle") end),
   awful.key({ modkey,           }, "a",      function() awful.util.spawn("/usr/bin/keepass2 --auto-type") end),
   
   -- tag navigation
   awful.key({ modkey,           }, "p",      awful.tag.viewprev       ),
   awful.key({ modkey,           }, "n",      awful.tag.viewnext       ),
   awful.key({ modkey,           }, "Up",     awful.tag.viewprev       ),
   awful.key({ modkey,           }, "Down",   awful.tag.viewnext       ),
   awful.key({ modkey,           }, "Escape", awful.tag.history.restore),
   awful.key({ modkey,           }, "e",      revelation               ),

   awful.key({ modkey,           }, "j",
             function ()
                awful.client.focus.byidx( 1)
                if client.focus then client.focus:raise() end
   end),
   awful.key({ modkey,           }, "k",
             function ()
                awful.client.focus.byidx(-1)
                if client.focus then client.focus:raise() end
   end),
   awful.key({ modkey,           }, "w", function () mymainmenu:show({keygrabber=true}) end),

   -- Layout manipulation
   awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end),
   awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end),
   awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end),
   awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end),
   awful.key({ modkey,           }, "u", awful.client.urgent.jumpto),
   awful.key({ modkey,           }, "Tab",
             function ()
                awful.client.focus.history.previous()
                if client.focus then
                   client.focus:raise()
                end
   end),

   -- Standard program
   awful.key({ modkey,           }, "Return", function () awful.util.spawn("x-terminal-emulator") end),
   awful.key({ modkey, "Control" }, "r", awesome.restart),

   awful.key({ modkey,           }, "Right", function () awful.tag.incmwfact( 0.05)    end),
   awful.key({ modkey,           }, "Left",  function () awful.tag.incmwfact(-0.05)    end),
   awful.key({ modkey, "Shift"   }, "Left",  function () awful.tag.incnmaster( 1)      end),
   awful.key({ modkey, "Shift"   }, "Right", function () awful.tag.incnmaster(-1)      end),
   awful.key({ modkey, "Control" }, "Left",  function () awful.tag.incncol( 1)         end),
   awful.key({ modkey, "Control" }, "Right", function () awful.tag.incncol(-1)         end),
   awful.key({ modkey,           }, "space", function () awful.layout.inc(layouts,  1) end),
   awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(layouts, -1) end),

   awful.key({ modkey, "Control" }, "n", awful.client.restore)
)

clientkeys = awful.util.table.join(
   awful.key({ modkey,           }, "f",      function (c) c.fullscreen = not c.fullscreen  end),
   awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end),
   awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ),
   awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end),
   awful.key({ modkey,           }, "o",      awful.client.movetoscreen                        ),
   awful.key({ modkey, "Shift"   }, "r",      function (c) c:redraw()                       end),
   awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end),
   -- awful.key({ modkey,           }, "n",
   --           function (c)
   --              -- The client currently has the input focus, so it cannot be
   --              -- minimized, since minimized clients can't have the focus.
   --              c.minimized = true
   --           end),
   awful.key({ modkey,           }, "m",
             function (c)
                c.maximized_horizontal = not c.maximized_horizontal
                c.maximized_vertical   = not c.maximized_vertical
   end)
)

-- Compute the maximum number of digit we need, limited to 9
keynumber = 0
for s = 1, screen.count() do
   keynumber = math.min(9, math.max(#tags[s], keynumber));
end

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, keynumber do
   globalkeys = awful.util.table.join(globalkeys,
                                      awful.key({ modkey }, "#" .. i + 9,
                                                function ()
                                                   local screen = mouse.screen
                                                   if tags[screen][i] then
                                                      awful.tag.viewonly(tags[screen][i])
                                                   end
                                      end),
                                      awful.key({ modkey, "Control" }, "#" .. i + 9,
                                                function ()
                                                   local screen = mouse.screen
                                                   if tags[screen][i] then
                                                      awful.tag.viewtoggle(tags[screen][i])
                                                   end
                                      end),
                                      awful.key({ modkey, "Shift" }, "#" .. i + 9,
                                                function ()
                                                   if client.focus and tags[client.focus.screen][i] then
                                                      awful.client.movetotag(tags[client.focus.screen][i])
                                                   end
                                      end),
                                      awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                                                function ()
                                                   if client.focus and tags[client.focus.screen][i] then
                                                      awful.client.toggletag(tags[client.focus.screen][i])
                                                   end
   end))
end

clientbuttons = awful.util.table.join(
   awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
   awful.button({ modkey }, 1, awful.mouse.client.move),
   awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)

--=================================================================================
--==================================== Rules ======================================
--=================================================================================

awful.rules.rules = {
   -- All clients will match this rule.
   { rule = { },
     properties = { border_width = beautiful.border_width,
                    border_color = beautiful.border_normal,
                    focus = true,
                    keys = clientkeys,
                    buttons = clientbuttons } },
   { rule = { class = "MPlayer" },
     properties = { floating = true } },
   { rule = { class = "pinentry" },
     properties = { floating = true } },
   { rule = { class = "gimp" },
     properties = { floating = true } },
   -- Set Firefox to always map on tags number 2 of screen 1.
   -- { rule = { class = "Firefox" },
   --   properties = { tag = tags[1][2] } },
}

--=================================================================================
--================================== Signals ======================================
--=================================================================================

-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c, startup)
                         -- Add a titlebar
                         -- awful.titlebar.add(c, { modkey = modkey })

                         -- -- Enable sloppy focus
                         -- c:connect_signal("mouse::enter", function(c)
                         --                 if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
                         --                    and awful.client.focus.filter(c) then
                         --                 client.focus = c
                         --                 end
                         --                              end)

                         if not startup then
                            -- Set the windows at the slave,
                            -- i.e. put it at the end of others instead of setting it master.
                            -- awful.client.setslave(c)

                            -- Put windows in a smart way, only if they does not set an initial position.
                            if not c.size_hints.user_position and not c.size_hints.program_position then
                               awful.placement.no_overlap(c)
                               awful.placement.no_offscreen(c)
                            end
                         end
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)

--=================================================================================
--=================================== Autoruns ====================================
--=================================================================================

-- start keyboard shortcut keys
awful.util.spawn_with_shell("$HOME/.local/bin/run-once.sh xbindkeys")

-- should set caps lock to be a control key
awful.util.spawn_with_shell("xmodmap ~/.xmodmap.conf")

-- start dropbox daemon
awful.util.spawn_with_shell("dropbox start")

-- disable touchpad tap-to-click
awful.util.spawn_with_shell("synclient MaxTapTime=0")

-- start gnome-do at login
awful.util.spawn_with_shell("$HOME/.local/bin/run-once.sh gnome-do")

-- grab a desktop image and display it
awful.util.spawn_with_shell("$HOME/dotfiles/nasa_iotd.sh")

-- Local Variables:
-- mode: lua
-- End:
