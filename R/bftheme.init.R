
#' Initial set up for the BFTheme package that installs the fonts
#'
#' @return Set up for bftheme
#' @examples
#' bftheme.init()

bftheme.init <- function(){
  ##########################
  #Add fonts - Rooney Pro
  ##########################
  sysfonts::font_add("RooneyPro-Regular",stringr::str_c(.libPaths(),"/BFTheme/extdata/font_files/Rooney_Pro/RooneyPro-Regular.otf"))
  sysfonts::font_add("RooneyPro-RegularItalic",stringr::str_c(.libPaths(),"/BFTheme/extdata/font_files/Rooney_Pro/RooneyPro-RegularItalic.otf"))
  sysfonts::font_add("RooneyPro-Bold",stringr::str_c(.libPaths(),"/BFTheme/extdata/font_files/Rooney_Pro/RooneyPro-Bold.otf"))
  sysfonts::font_add("RooneyPro-BoldItalic",stringr::str_c(.libPaths(),"/BFTheme/extdata/font_files/Rooney_Pro/RooneyPro-BoldItalic.otf"))
  sysfonts::font_add("RooneyPro-Medium",stringr::str_c(.libPaths(),"/BFTheme/extdata/font_files/Rooney_Pro/RooneyPro-Medium.otf"))
  sysfonts::font_add("RooneyPro-MediumItalic",stringr::str_c(.libPaths(),"/BFTheme/extdata/font_files/Rooney_Pro/RooneyPro-MediumItalic.otf"))
  sysfonts::font_add("RooneyPro-Light",stringr::str_c(.libPaths(),"/BFTheme/extdata/font_files/Rooney_Pro/RooneyPro-Light.otf"))
  sysfonts::font_add("RooneyPro-LightItalic",stringr::str_c(.libPaths(),"/BFTheme/extdata/font_files/Rooney_Pro/RooneyPro-LightItalic.otf"))
  ##########################
  #Add fonts - Rooney Sans
  ##########################
  sysfonts::font_add("RooneySans-Regular",stringr::str_c(.libPaths(),"/BFTheme/extdata/font_files/Rooney_Sans/RooneySans-Regular.otf"))
  sysfonts::font_add("RooneySans-RegularItalic",stringr::str_c(.libPaths(),"/BFTheme/extdata/font_files/Rooney_Sans/RooneySans-RegularItalic.otf"))
  sysfonts::font_add("RooneySans-Bold",stringr::str_c(.libPaths(),"/BFTheme/extdata/font_files/Rooney_Sans/RooneySans-Bold.otf"))
  sysfonts::font_add("RooneySans-BoldItalic",stringr::str_c(.libPaths(),"/BFTheme/extdata/font_files/Rooney_Sans/RooneySans-BoldItalic.otf"))
  sysfonts::font_add("RooneySans-Medium",stringr::str_c(.libPaths(),"/BFTheme/extdata/font_files/Rooney_Sans/RooneySans-Medium.otf"))
  sysfonts::font_add("RooneySans-MediumItalic",stringr::str_c(.libPaths(),"/BFTheme/extdata/font_files/Rooney_Sans/RooneySans-MediumItalic.otf"))
  sysfonts::font_add("RooneySans-Light",stringr::str_c(.libPaths(),"/BFTheme/extdata/font_files/Rooney_Sans/RooneySans-Light.otf"))
  sysfonts::font_add("RooneySans-LightItalic",stringr::str_c(.libPaths(),"/BFTheme/extdata/font_files/Rooney_Sans/RooneySans-LightItalic.otf"))
  ##########################
  #Add fonts - Pressura
  ##########################
  sysfonts::font_add("GT-Pressura-Regular",stringr::str_c(.libPaths(),"/BFTheme/extdata/font_files/Pressura/GT-Pressura-Regular.otf"))
  sysfonts::font_add("GT-Pressura-RegularItalic",stringr::str_c(.libPaths(),"/BFTheme/extdata/font_files/Pressura/GT-Pressura-RegularItalic.otf"))
  sysfonts::font_add("GT-Pressura-Bold",stringr::str_c(.libPaths(),"/BFTheme/extdata/font_files/Pressura/GT-Pressura-Bold.otf"))
  sysfonts::font_add("GT-Pressura-BoldItalic",stringr::str_c(.libPaths(),"/BFTheme/extdata/font_files/Pressura/GT-Pressura-BoldItalic.otf"))
  sysfonts::font_add("GT-Pressura-Light",stringr::str_c(.libPaths(),"/BFTheme/extdata/font_files/Pressura/GT-Pressura-Light.otf"))
  sysfonts::font_add("GT-Pressura-LightItalic",stringr::str_c(.libPaths(),"/BFTheme/extdata/font_files/Pressura/GT-Pressura-LightItalic.otf"))
  ##########################
  #Add fonts - Entypo
  ##########################
  sysfonts::font_add("Entypo",stringr::str_c(.libPaths(),"/BFTheme/extdata/font_files/Entypo/Entypo-Social.otf"))
  showtext::showtext_auto()

}
