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
      XConfig(focusFollowsMouse, modMask, terminal, workspaces, borderWidth,
              normalBorderColor, focusedBorderColor, handleEventHook, manageHook,
              layoutHook, logHook, startupHook) )
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
import qualified XMonad.DBus as D
import qualified DBus.Client as DC

main :: IO ()
main = do
  -- dbus <- D.connect
  -- D.requestAccess dbus
  xmonad $ ewmhFullscreen $ gnomeConfig
    { modMask = myModMask
    -- , logHook = dynamicLogWithPP (myLogHook dbus)
    , terminal = "wezterm"
    , workspaces = myWorkspaces
    , borderWidth = 4
    , focusFollowsMouse = True
    , layoutHook = spacingRaw True (Border 0 0 0 0) False (Border 6 6 6 6) True myLayout
    , normalBorderColor = "#707880"
    -- , focusedBorderColor = "#2980b9"
    , focusedBorderColor = "#33AADD"
    -- , focusedBorderColor = "#A54242"
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
       ,((myModMask, xK_p), spawn myLauncher)
       -- ,((myModMask .|. shiftMask, xK_p), spawn "albert show")
       -- ,((mod1Mask, xK_F2), spawn "albert show")
       ,((myModMask, xK_x), spawn "flameshot full -c")
       ,((myModMask .|. shiftMask, xK_x), spawn "flameshot gui")
       ,((myModMask, xK_backslash), spawn "1password --toggle")
       ,((mod1Mask .|. controlMask, xK_l), spawn "dbus-send --type=method_call --dest=org.gnome.ScreenSaver /org/gnome/ScreenSaver org.gnome.ScreenSaver.Lock")
       -- ,((myModMask, xK_x), spawn "gnome-screenshot -i")
       -- ,((myModMask .|. shiftMask, xK_x), spawn "gnome-screenshot -a -i")
       ]
    )

myLogHook :: DC.Client -> PP
myLogHook dbus = def { ppOutput = D.send dbus }

myLayout = smartBorders $ layoutHook gnomeConfig ||| desktopLayoutModifiers (ThreeColMid 1 (3/100) (1/2))

myLauncher = "$($HOME/.cabal/bin/yeganesh -x -- -fn 'Monoid-8' -b)"

myModMask = mod4Mask

myStartupHook = do
  startupHook gnomeConfig
  setWMName "LG3D"
  spawn "$HOME/bin/xmonad-post.sh"

myManageHook = composeAll
  [ className =? "mpv" --> doFloat
  , className =? "MComix" --> doFloat
  , className =? "Gnome-mpv" --> doFloat
  , className =? "Gnome-tweaks" --> doFloat
  , className =? "Gnome-control-center" --> doFloat
  , className =? "EasyConnect" --> doFloat
  , className =? "Ulauncher" --> hasBorder False >> doFloat
  , className =? "albert" --> hasBorder False >> doFloat
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
