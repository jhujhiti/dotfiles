import XMonad
import XMonad.Config.Gnome
import XMonad.Util.EZConfig
import qualified Data.Map as M
import XMonad.Layout.PerWorkspace
import XMonad.Layout.TwoPane
import XMonad.Layout.Combo (combineTwo)
import XMonad.Layout.OneBig
import XMonad.Layout.LayoutHints (layoutHints)
import XMonad.Hooks.ManageDocks (avoidStruts)

myKeys = [
    ("M-S-q", spawn "gnome-session-save --gui --logout-dialog"),
    ("M-S-v", spawn "gvim"),
    ("M-S-l", spawn "gnome-screensaver-command --lock"),
    ("M-S-b", spawn "gnome-open http:///")
    ]

defaultLayout = layoutHints $ tiled ||| Mirror tiled ||| Full
    where
        tiled = Tall nmaster delta ratio
        nmaster = 1
        delta = 3/100
        ratio = 1/2

imLayout = combineTwo ((TwoPane) (3/100) (13/16)) defaultLayout (OneBig 1 1)

myLayoutHook = onWorkspace "im" imLayout $ defaultLayout

myWorkspaces = map show [1 .. 8] ++ ["im"]

main :: IO()
main = xmonad $ gnomeConfig {
    modMask = mod4Mask,
    terminal = "urxvt",
    normalBorderColor = "#303030",
    focusedBorderColor = "#c45679",
    borderWidth = 2,
    layoutHook = avoidStruts myLayoutHook,
    workspaces = myWorkspaces
    }
    `additionalKeysP` myKeys
