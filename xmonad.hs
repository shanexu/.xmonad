import XMonad
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

main :: IO ()
main = xmonad $ gnomeConfig
  { modMask = myModMask
  -- , terminal = "terminator"
  , workspaces = myWorkspaces
  , borderWidth = 2
  -- , layoutHook = smartBorders $ spacingRaw True (Border 2 2 2 2) True (Border 2 2 2 2) True $ layoutHook gnomeConfig
  , layoutHook = smartBorders $ layoutHook gnomeConfig
  , normalBorderColor = "#777777"
  , focusedBorderColor = "#2980b9"
  , handleEventHook = handleEventHook gnomeConfig <+> fullscreenEventHook
  , startupHook = myStartupHook
  , manageHook = myManageHook <+> manageHook gnomeConfig
  } `additionalKeys` (
  [((myModMask, key), windows $ W.greedyView ws) | (key, ws) <- myExtraWorkspaces ]
  ++ [((myModMask .|. shiftMask, key), windows $ W.shift ws) | (key, ws) <- myExtraWorkspaces]
  ++ [((m .|. myModMask, key), screenWorkspace sc >>= flip whenJust (windows . f)) -- Replace 'mod1Mask' with your mod key of choice.
  | (key, sc) <- zip [xK_w, xK_e, xK_r] [2,0,1] -- was [0..] *** change to match your screen order ***
  , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
  ++ [((myModMask, xK_g), withFocused toggleBorder)]
  )

myModMask = mod4Mask

myStartupHook = do
  startupHook gnomeConfig
  setWMName "LG3D"

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
  , title =? "Run Application" --> doFloat
  , title =? "Log Out" --> doFloat
  , isFullscreen --> doFullFloat
  ]

myWorkspaces = miscs 9 ++ ["0"] -- ["web", "terms", "editor"] ++ (miscs 6) ++ ["mail"]
  where miscs = map (("" ++) . show) . flip take [1..]

myExtraWorkspaces = [(xK_0, "0")] -- list of (key, name)
