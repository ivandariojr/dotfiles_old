module Main (main) where

import System.IO

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Actions.CycleWS


main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ defaultConfig { 
    workspaces = ["ff1", "term", "emacs", "4", "5", "keepas", "im", "email", "ff2"],
    focusFollowsMouse = False,
    borderWidth = 2,
    terminal = "terminator",
    manageHook = manageDocks <+> manageHook defaultConfig,
    layoutHook = avoidStruts  $  layoutHook defaultConfig,
    logHook = dynamicLogWithPP xmobarPP {
      ppOutput = hPutStrLn xmproc,
      ppCurrent = xmobarColor "green" "" . wrap "<" ">"
      },
    modMask = mod1Mask .|. controlMask
    }
    `additionalKeys`
    [
      ((mod1Mask .|. controlMask, xK_semicolon), spawn "gnome-screensaver-command --lock"),
      ((mod1Mask .|. controlMask .|. shiftMask, xK_Up), shiftToPrev >> prevWS),
      ((mod1Mask .|. controlMask .|. shiftMask, xK_Down), shiftToNext >> nextWS),
      ((mod1Mask .|. controlMask, xK_Up), prevWS),
      ((mod1Mask .|. controlMask, xK_Down), nextWS)
    ]
