/*
 Bug 931@github reported by lvc on 24/05/2016.
 
 The latest version (9320ecf) of the universal ctags failed to recognize
 SetImageChannelDepth symbol in the ImageMagick library header file
 (include/ImageMagick-6/magick/attribute.h, v6.9.4-4):
 
 In fact only the last function prototype was extracted and everything
 else was treated as a giant return type.
*/

extern MagickExport MagickBooleanType
  IdentifyImageMonochrome(const Image *,ExceptionInfo *),
  IsGrayImage(const Image *,ExceptionInfo *),
  IsMonochromeImage(const Image *,ExceptionInfo *),
  IsOpaqueImage(const Image *,ExceptionInfo *),
  SetImageChannelDepth(Image *,const ChannelType,const size_t),
  SetImageDepth(Image *,const size_t),
  SetImageType(Image *,const ImageType);
  