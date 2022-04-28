import XMonad
    ( controlMask,
      mod1Mask,
      mod4Mask,
      shiftMask,
      xK_0,
      xK_F2,
      xK_backslash,
      xK_e,
      xK_g,
      xK_l,
      xK_p,
      xK_r,
      xK_w,
      xK_x,
      spawn,
      whenJust,
      (|||),
      xmonad,
      (-->),
      (<+>),
      (=?),
      className,
      composeAll,
      doFloat,
      title,
      screenWorkspace,
      windows,
      withFocused,
      (.|.),
      XConfig(modMask, terminal, workspaces, borderWidth,
              normalBorderColor, focusedBorderColor, handleEventHook, manageHook,
              layoutHook, startupHook) )
import XMonad.Config.Gnome
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import System.Environment
import qualified XMonad.StackSet as W
import XMonad.Actions.NoBorders
import XMonad.Layout.ThreeColumns
import System.Directory

main :: IO ()
main = do
  home <- getHomeDirectory
  xmonad $ ewmhFullscreen $ gnomeConfig
    { modMask = myModMask
    , terminal = "wezterm"
    , workspaces = myWorkspaces
    , borderWidth = 4
    -- , layoutHook = smartBorders $ spacingRaw True (Border 2 2 2 2) True (Border 2 2 2 2) True $ layoutHook gnomeConfig
    , layoutHook = myLayout
    , normalBorderColor = "#777777"
    , focusedBorderColor = "#2980b9"
    , handleEventHook = handleEventHook gnomeConfig
    , startupHook = myStartupHook
    , manageHook = myManageHook <+> manageHook gnomeConfig
    } `additionalKeys` (
    [((myModMask, key), windows $ W.greedyView ws) | (key, ws) <- myExtraWorkspaces ]
    ++ [((myModMask .|. shiftMask, key), windows $ W.shift ws) | (key, ws) <- myExtraWorkspaces]
    ++ [((m .|. myModMask, key), screenWorkspace sc >>= flip whenJust (windows . f)) -- Replace 'mod1Mask' with your mod key of choice.
       | (key, sc) <- zip [xK_w, xK_e, xK_r] [2,0,1] -- was [0..] *** change to match your screen order ***
       , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    ++ [
        ((myModMask, xK_g), withFocused toggleBorder)
       ,((myModMask, xK_p), spawn ("$(" ++ home ++ "/.cabal/bin/yeganesh -x -- -fn 'JetBrains Mono NL-9' -b)"))
       ,((myModMask .|. shiftMask, xK_p), gnomeRun)
       ,((mod1Mask, xK_F2), gnomeRun)
       ,((myModMask, xK_x), spawn "flameshot full -c")
       ,((myModMask .|. shiftMask, xK_x), spawn "flameshot gui")
       ,((myModMask, xK_backslash), spawn "1password --toggle")
       ,((mod1Mask .|. controlMask, xK_l), spawn "dbus-send --type=method_call --dest=org.gnome.ScreenSaver /org/gnome/ScreenSaver org.gnome.ScreenSaver.Lock")
       -- ,((myModMask, xK_x), spawn "gnome-screenshot -i")
       -- ,((myModMask .|. shiftMask, xK_x), spawn "gnome-screenshot -a -i")
       ]
    )

myLayout = smartBorders $ layoutHook gnomeConfig ||| desktopLayoutModifiers (ThreeColMid 1 (3/100) (1/2))

myLauncher = "$(/home/shane/.calbal/bin/yeganesh -x -- -fn 'dina-9' -b)"

myModMask = mod4Mask

myStartupHook = do
  startupHook gnomeConfig
  setWMName "LG3D"
  spawn "/home/shane/bin/gnome-panel-replace.sh"

myManageHook = composeAll
  [ className =? "mpv" --> doFloat
  , className =? "MComix" --> doFloat
  , className =? "Gnome-mpv" --> doFloat
  , className =? "Gnome-tweaks" --> doFloat
  , className =? "Gnome-control-center" --> doFloat
  , className =? "EasyConnect" --> doFloat
  , className =? "Kupfer" --> doFloat
  , className =? "albert" --> doFloat
  , className =? "Synapse" --> doFloat
  , className =? "netease-cloud-music" --> doFloat
  , className =? "Gnome-screenshot" --> doFloat
  , className =? "1Password" --> doFloat
  , className =? "Youdao Dict" --> hasBorder False >> doFloat
  , title =? "Run Application" --> doFloat
  , title =? "Log Out" --> doFloat
  , isFullscreen --> doFullFloat
  ]

myWorkspaces = miscs 9 ++ ["0"]
  where miscs = map (("" ++) . show) . flip take [1..]

myExtraWorkspaces = [(xK_0, "0")]
