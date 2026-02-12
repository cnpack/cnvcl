{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     ÖĞ¹úÈË×Ô¼ºµÄ¿ª·ÅÔ´ÂëµÚÈı·½¿ª·¢°ü                         }
{                   (C)Copyright 2001-2026 CnPack ¿ª·¢×é                       }
{                   ------------------------------------                       }
{                                                                              }
{            ±¾¿ª·¢°üÊÇ¿ªÔ´µÄ×ÔÓÉÈí¼ş£¬Äú¿ÉÒÔ×ñÕÕ CnPack µÄ·¢²¼Ğ­ÒéÀ´ĞŞ        }
{        ¸ÄºÍÖØĞÂ·¢²¼ÕâÒ»³ÌĞò¡£                                                }
{                                                                              }
{            ·¢²¼ÕâÒ»¿ª·¢°üµÄÄ¿µÄÊÇÏ£ÍûËüÓĞÓÃ£¬µ«Ã»ÓĞÈÎºÎµ£±£¡£ÉõÖÁÃ»ÓĞ        }
{        ÊÊºÏÌØ¶¨Ä¿µÄ¶øÒşº¬µÄµ£±£¡£¸üÏêÏ¸µÄÇé¿öÇë²ÎÔÄ CnPack ·¢²¼Ğ­Òé¡£        }
{                                                                              }
{            ÄúÓ¦¸ÃÒÑ¾­ºÍ¿ª·¢°üÒ»ÆğÊÕµ½Ò»·İ CnPack ·¢²¼Ğ­ÒéµÄ¸±±¾¡£Èç¹û        }
{        »¹Ã»ÓĞ£¬¿É·ÃÎÊÎÒÃÇµÄÍøÕ¾£º                                            }
{                                                                              }
{            ÍøÕ¾µØÖ·£ºhttps://www.cnpack.org                                  }
{            µç×ÓÓÊ¼ş£ºmaster@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnGraphics;
{* |<PRE>
================================================================================
* Èí¼şÃû³Æ£º½çÃæ¿Ø¼ş°ü
* µ¥ÔªÃû³Æ£º½çÃæ¿Ø¼ş°üÔ­¿ìËÙÍ¼Ïñ´¦Àíµ¥Ôª
* µ¥Ôª×÷Õß£ºÖÜ¾¢Óğ (zjy@cnpack.org)
* ±¸    ×¢£º¸Ãµ¥ÔªÎª¾ÉµÄÍ¼Ïñ¿â£¬ĞÂÍ¼Ïñ¿âÕıÔÚÖØĞÂÖÆ×÷ÖĞ
* ¿ª·¢Æ½Ì¨£ºPWin98SE + Delphi 5.0
* ¼æÈİ²âÊÔ£ºPWin9X/2000/XP + Delphi 5/6
* ±¾ µØ »¯£º¸Ãµ¥ÔªÖĞµÄ×Ö·û´®¾ù·ûºÏ±¾µØ»¯´¦Àí·½Ê½
* ĞŞ¸Ä¼ÇÂ¼£º2026.02.13 V0.13
*               Ôö¼Ó TCnCOlorSpaceConverter£¬ÔİÎ´ÍêÕû²âÊÔ
*           2022.09.25 V0.12
*               Ôö¼Ó Win64 Î»µÄÖ§³Ö
*           2002.03.14 V0.11Alpha
*               Ô­Í¼Ïñ´¦Àí¿â×îºó°æ±¾
*               ĞÂ¿âÕıÔÚÖÆ×÷ÖĞ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, SysUtils, Classes, Graphics, ExtCtrls, Math, Controls, Messages,
  CnNative, CnClasses, CnCommon, CnGraphConsts, CnMatrix;

type

//--------------------------------------------------------//
// Í¼Ïñ´¦Àí»ù±¾ÀàĞÍ¶¨Òå                                   //
//--------------------------------------------------------//

  PCnColor = ^TCnColor;
  {* Ö¸Ïò TCnColor µÄÖ¸ÕëÀàĞÍ}
  TCnColor = packed record
  {* 24Î»ÑÕÉ«Öµ¼ÇÂ¼}
    b, g, r: Byte;
  end;

  PCnLine = ^TCnLine;
  {* Ö¸Ïò TCnLine µÄÖ¸ÕëÀàĞÍ£¬Ò»°ãÖ¸ÏòÒ»ĞĞÎ»Í¼É¨ÃèÏßÊı¾İ}
  TCnLine = array[0..65535] of TCnColor;
  {* TCnColor Êı×éÀàĞÍ£¬Ò»°ãÎªÒ»ĞĞÎ»Í¼É¨ÃèÏßÊı¾İ}
  PPCnLines = ^TPCnLines;
  {* Ö¸Ïò TPCnLines µÄÖ¸ÕëÀàĞÍ£¬Ò»°ãÓÃÀ´Ö¸ÏòÎ»Í¼É¨ÃèÏßµØÖ·Êı×é}
  TPCnLines = array[0..65535] of PCnLine;
  {* PCnLine Êı×éÀàĞÍ£¬Ò»°ãÓÃÓÚ´æ´¢Î»Í¼É¨ÃèÏßµØÖ·Êı×é}

  TFilterCore = array[0..2, 0..2] of SmallInt;
  {* ÓÃÓÚ 3x3 Í¼Ïñ¾í»ı´¦ÀíµÄ¾í»ıºË³ÂÁĞ}

  TPointF = record
  {* ¸¡µãÊı×ø±ê¼ÇÂ¼ÀàĞÍ}
    x, y: Single;
  end;

  TPointFArray = array of TPointF;
  {* TPointF ¸¡µãÊı×ø±ê¶¯Ì¬Êı×é£¬Ò»°ãÓÃÓÚÍ¼ĞÎ»æÖÆ²ÎÊı´«µİ}

  TRectF = record
  {* ¸¡µãÊı¾ØĞÎ¼ÇÂ¼ÀàĞÍ}
    case Integer of
      0: (Left, Top, Right, Bottom: Single);
      1: (TopLeft, BottomRight: TPointF);
  end;

  TCnDrawMode = (dmDraw, dmCenter, dmStretched, dmResize, dmTiled);
  {* Í¼Ïñ»æÖÆÄ£Ê½
   |<BR>
   |<BR>    dmDraw       ½«Ô´Í¼ÏñÖ±½Ó»æÖÆÔÚÄ¿±êÍ¼Ïñ×óÉÏ½Ç
   |<BR>    dmCenter     ½«Ô´Í¼Ïñ»æÖÆµ½Ä¿±êÍ¼ÏñÖĞĞÄÎ»ÖÃ
   |<BR>    dmStretched  ½«Ô´Í¼ÏñËõ·Å»æÖÆµ½Ä¿±êÍ¼Ïñ
   |<BR>    dmResize     Ëõ·Å»æÖÆÊ±±£³ÖÔ´Í¼Ïñ³¤¿í±È
   |<BR>    dmTiled      Ô´Í¼ÏñÆ½ÆÌ»æÖÆµ½Ä¿±êÍ¼Ïñ
  }

const
  csMaxAlpha = High(Byte);
  csMaxProgress = 100;

type
  TCnAlpha = Byte;
  {* Í¼Ïñ Alpha »ìºÏÏµÊı 0..255£¬0 ±íÊ¾È«Í¸Ã÷£¬255 ±íÊ¾²»Í¸Ã÷}
  TCnProgress = 0..csMaxProgress;
  {* ´¦Àí½ø³Ì°Ù·Ö±ÈÀàĞÍ 0..100}

//--------------------------------------------------------//
// Í¼Ïñ´¦ÀíÒì³£ÀàĞÍ¶¨Òå                                   //
//--------------------------------------------------------//

type
  ECnGraphics = class(Exception);
  {* CnPack ¿ìËÙÍ¼Ïñ¿âÒì³£»ùÀà}

  EInvalidPixel = class(ECnGraphics);
  {* ÎŞĞ§µÄÏñËØµãÒì³££¬Í¨³£ÊÇÒòÎª·ÃÎÊ TCnBitmap µÄÏñËØµãÊ±·¶Î§³¬½ç}
  EInvalidScanLine = class(ECnGraphics);
  {* ÎŞĞ§µÄÉ¨ÃèÏßÒì³££¬Í¨³£ÊÇÒòÎª·ÃÎÊ TCnBitmap µÄÉ¨ÃèÏßÊ±·¶Î§³¬½ç}
  EBitmapIsEmpty = class(ECnGraphics);
  {* ÎŞ·¨·ÃÎÊ¿ÕÎ»Í¼Òì³££¬Í¨³£ÊÇÒòÎª·ÃÎÊÃ»ÓĞÎ»Í¼Êı¾İµÄ TCnBitmap µÄÏñËØ»òÉ¨ÃèÏß}
  EInvalidForeBmp = class(ECnGraphics);
  {* ÎŞĞ§µÄÇ°¾°Í¼Òì³££¬ÔÚ»æÖÆÆ½»¬×ÖÌåÊ±²úÉú£¬ÄÚ²¿µ÷ÊÔÓÃ}

//--------------------------------------------------------//
// ½¥±äÑÕÉ«Àà                                             //
//--------------------------------------------------------//

const
  csMaxGradPos = 100;

type
  TCnGradPos = 0..csMaxGradPos;
  {* ½¥±äÑÕÉ«Î»ÖÃÀàĞÍ 0..100£¬ÓÃÓÚ±êÊ¶Ò»¸öÖĞ¼äÑÕÉ«ÔÚ½¥±äÉ«´øÖĞµÄÎ»ÖÃ}
  TCnGradStyle = (gsLeftToRight, gsRightToLeft, gsTopToBottom, gsBottomToTop,
    gsCenterToLR, gsCenterToTB, gsRadial);
  {* ½¥±äÉ«»æÖÆÄ£Ê½
   |<BR>
   |<BR>    gsLeftToRight     ´Ó×óµ½ÓÒ½¥±ä
   |<BR>    gsRightToLeft     ´ÓÓÒµ½×ó½¥±ä
   |<BR>    gsTopToBottom     ´ÓÉÏµ½ÏÂ½¥±ä
   |<BR>    gsBottomToTop     ´ÓÏÂµ½ÉÏ½¥±ä
   |<BR>    gsCenterToLR      ´ÓÖĞ¼äµ½Á½±ß½¥±ä
   |<BR>    gsCenterToTB      ´ÓÖĞ¼äµ½ÉÏÏÂ½¥±ä
   |<BR>    gsRadial          ´ÓÖĞ¼äËÄÖÜ·øÉä½¥±ä
  }
  TCnMiddleColor = class;

{ TCnMiddleColorItem }

  TCnMiddleColorItem = class(TCollectionItem)
  {* ½¥±äÑÕÉ«´øµÄÖĞ¼äÉ«×ÓÏîÀà}
  private
    FColor: TColor;
    FPos: TCnGradPos;
    procedure SetColor(const Value: TColor);
    procedure SetPos(const Value: TCnGradPos);
  public
    constructor Create(Collection: TCollection); override;
    {* ¹¹ÔìÆ÷£¬Ó¦´«µİÒ»¸ö TCnMiddleColor ¶ÔÏó×ö²ÎÊı£¬Ò»°ã²»ĞèÒªÊÖ¶¯µ÷ÓÃ}
    procedure Assign(Source: TPersistent); override;
    {* ¸³Öµ·½·¨£¬Ò»°ã²»ĞèÒªÊÖ¶¯µ÷ÓÃ}
  published
    property Color: TColor read FColor write SetColor;
    {* ½¥±äÑÕÉ«´øµÄÖĞ¼äÑÕÉ«Öµ}
    property Pos: TCnGradPos read FPos write SetPos;
    {* ½¥±äÑÕÉ«´øµÄÖĞ¼äÑÕÉ«ÔÚÉ«´øÖĞµÄÎ»ÖÃ}
  end;

{ TCnMiddleColor }

  TCnMiddleColor = class(TOwnedCollection)
  {* ½¥±äÑÕÉ«´øµÄÖĞ¼äÉ«ÊÕ¼¯Æ÷Àà£¬Ö÷ÒªÓÃÓÚ TCnGradientColor ¶ÔÏóÖĞ}
  private
    FSorting: Boolean;
    function GetItem(Index: Integer): TCnMiddleColorItem;
    procedure SetItem(Index: Integer; const Value: TCnMiddleColorItem);
  protected
    procedure Update(Item: TCollectionItem); override;
    function GetAttrCount: Integer; override;
    function GetAttr(Index: Integer): string; override;
    function GetItemAttr(Index, ItemIndex: Integer): string; override;
  public
    constructor Create(AOwner: TPersistent);
    {* ¹¹ÔìÆ÷£¬ÓÃÓÚ´´½¨Ò»¸ö¸ÃÀàµÄÊµÀı}
    procedure Add(AColor: TColor; APos: TCnGradPos);
    {* Ôö¼Ó×ÓÏî·½·¨£¬ÓÃÓÚÏòÖĞ¼äÉ«´øÖĞÔö¼ÓÒ»¸öÑÕÉ«Ïî
     |<BR>
     |<BR>    AColor: TColor      Ôö¼ÓµÄÑÕÉ«Öµ
     |<BR>    APos: TCnGradPos    ¸ÃÑÕÉ«ÔÚ½¥±äÉ«´øÖĞµÄÎ»ÖÃ
    }
    procedure Sort;
    {* ¶Ô½¥±äÉ«°´Î»ÖÃÅÅĞò£¬Ò»°ã²»ĞèÒªÊÖ¶¯µ÷ÓÃ}
    property Items[Index: Integer]: TCnMiddleColorItem read GetItem write SetItem;
    default;
    {* ½¥±äÉ«´øÑÕÉ«Ïî£¬ÔÊĞí°´Êı×éµÄĞÎÊ½·ÃÎÊÉ«´øÖĞµÄ×ÓÏî}
  end;

{ TCnGradientColor }

  TCnGradientColor = class(TCnPersistent)
  {* ½¥±äÑÕÉ«Àà£¬±£´æÁË½¥±äÉ«»æÖÆ²ÎÊı£¬¿É×÷Îª²ÎÊı´«µİ¸ø TCnBitmap µÄ½¥±äÉ«»æÖÆ·½Ê½}
  private
    FColorStart: TColor;
    FColorEnd: TColor;
    FStyle: TCnGradStyle;
    FColorMiddle: TCnMiddleColor;
    procedure SetColorEnd(const Value: TColor);
    procedure SetColorStart(const Value: TColor);
    procedure SetStyle(const Value: TCnGradStyle);
    function GetColorMiddle: TCnMiddleColor;
    procedure SetColorMiddle(const Value: TCnMiddleColor);
  public
    constructor Create; override;
    {* ¹¹ÔìÆ÷£¬ÓÃÓÚ´´½¨Ò»¸ö¸ÃÀàµÄÊµÀı}
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    {* ¸³Öµ·½·¨£¬Ò»°ã²»ĞèÒªÊÖ¶¯µ÷ÓÃ}
  published
    property ColorMiddle: TCnMiddleColor read GetColorMiddle write SetColorMiddle;
    {* ½¥±äÑÕÉ«µÄÖĞ¼äÉ«´øÏî}
    property ColorStart: TColor read FColorStart write SetColorStart default clBlack;
    {* ½¥±äÆğÊ¼É«}
    property ColorEnd: TColor read FColorEnd write SetColorEnd default clBlack;
    {* ½¥±ä½áÊøÉ«}
    property Style: TCnGradStyle read FStyle write SetStyle default gsLeftToRight;
    {* ½¥±ä·½Ê½}
  end;

//--------------------------------------------------------//
// Æ½»¬ÌØĞ§×ÖÌåÀà                                         //
//--------------------------------------------------------//

  TFontQuality = (fqHigh, fqNormal, fqLow, fqNone);
  {* Æ½»¬×ÖÌå»æÖÆ¾«¶ÈÀàĞÍ
   |<BR>
   |<BR>    fqHigh      ¸ß¾«¶È»æÖÆ£¬²ÉÓÃ4x4²ÉÑù£¬ËÙ¶È½ÏÂı
   |<BR>    fqNormal    ÆÕÍ¨¾«¶È»æÖÆ£¬²ÉÓÃ3x3²ÉÑù£¬×î¼ÑËÙ¶ÈÖÊÁ¿±È£¬Ä¬ÈÏÖµ
   |<BR>    fqLow       µÍ¾«¶È»æÖÆ£¬²ÉÓÃ2x2²ÉÑù£¬ËÙ¶È½Ï¿ì
   |<BR>    fqNone      ÎŞÆ½»¬Ğ§¹û
  }
  TFontStyleEx = (fsShadow, fsGradient, fsTexture, fsNoise, fsOutline,
    fsLighting, fsSpray);
  {* Æ½»¬×ÖÌåÀ©Õ¹ÌØĞ§·ç¸ñ
   |<BR>
   |<BR>    fsShadow      ÉèÖÃÒõÓ°Ğ§¹û£¬¼û TCnShadow
   |<BR>    fsGradient    ÉèÖÃÎÄ±¾ÑÕÉ«½¥±äĞ§¹û£¬¼û TCnGradientColor
   |<BR>    fsTexture     ÉèÖÃÎÄ±¾ÎÆÀíÍ¼
   |<BR>    fsNoise       ÉèÖÃÎÄ±¾ÔëÉùÎÆÀí
   |<BR>    fsOutline     ÉèÖÃÎÄ±¾ÒÔÂÖÀªÏß·½Ê½ÏÔÊ¾
   |<BR>    fsLighting    ÉèÖÃµÆ¹âĞ§¹û£¬¼û TCnLighting
   |<BR>    fsSpray       ÉèÖÃÅç½¦Ğ§¹û
  }
  TFontStyleExs = set of TFontStyleEx;
  {* Æ½»¬×ÖÌåÀ©Õ¹ÌØĞ§·ç¸ñ¼¯ºÏ}

{ TCnShadow }

  TShadowOffset = -20..20;
  {* ÒõÓ°Æ«ÒÆ·¶Î§}
  TShadowBlur = 0..10;
  {* ÒõÓ°Ä£ºı¶È}

  TCnShadow = class(TCnPersistent)
  {* ÒõÓ°²ÎÊıÀà£¬¶¨ÒåÓÃÓÚÆ½»¬×ÖÌå»òÍ¼ÏñÌØĞ§µÄÒõÓ°²ÎÊı}
  private
    FBlur: TShadowBlur;
    FAlpha: TCnAlpha;
    FColor: TColor;
    FOffsetX: TShadowOffset;
    FOffsetY: TShadowOffset;
    procedure SetBlur(const Value: TShadowBlur);
    procedure SetColor(const Value: TColor);
    procedure SetOffsetX(const Value: TShadowOffset);
    procedure SetOffsetY(const Value: TShadowOffset);
    procedure SetAlpha(const Value: TCnAlpha);
  public
    constructor Create; override;
    {* ¹¹ÔìÆ÷£¬ÓÃÓÚ´´½¨Ò»¸ö¸ÃÀàµÄÊµÀı}
    procedure Assign(Source: TPersistent); override;
    {* ¸³Öµ·½·¨£¬Ò»°ã²»ĞèÒªÊÖ¶¯µ÷ÓÃ}
  published
    property Blur: TShadowBlur read FBlur write SetBlur default 1;
    {* ÒõÓ°Ä£ºıÊôĞÔ£¬²ÉÓÃ3X3¸ßË¹Ä£ºıËã·¨½øĞĞÒõÓ°Ä£ºı´¦Àí}
    property Alpha: TCnAlpha read FAlpha write SetAlpha default 180;
    {* ÒõÓ°µÄ²»Í¸Ã÷¶ÈÊôĞÔ}
    property Color: TColor read FColor write SetColor default $00444444;
    {* ÒõÓ°ÑÕÉ«}
    property OffsetX: TShadowOffset read FOffsetX write SetOffsetX default 2;
    {* ÒõÓ°ÔÚË®Æ½·½ÏòµÄÆ«ÒÆÁ¿£¬·¶Î§Îª -20..20£¬Îª¸º±íÊ¾Ïò×óÆ«}
    property OffsetY: TShadowOffset read FOffsetY write SetOffsetY default 2;
    {* ÒõÓ°ÔÚ´¹Ö±·½ÏòµÄÆ«ÒÆÁ¿£¬·¶Î§Îª -20..20£¬Îª¸º±íÊ¾ÏòÉÏÆ«}
  end;

{ TCnShadow }

  TLightingOffset = -200..200;
  {* ¹âÕÕÖĞĞÄµãÆ«ÒÆ·¶Î§£¬°Ù·Ö±È£¬Îª¸º±íÊ¾×óÆ«}
  TLightingRange = 0..1000;
  {* ¹âÕÕ·¶Î§£¬°Ù·Ö±È}

  TCnLighting = class(TCnPersistent)
  {* ¹âÕÕĞ§¹û²ÎÊıÀà£¬¶¨ÒåÓÃÓÚÆ½»¬×ÖÌå»òÍ¼ÏñÌØĞ§µÄµÆ¹âĞ§¹û²ÎÊı}
  private
    FAlpha: TCnAlpha;
    FColor: TColor;
    FOffsetX: TLightingOffset;
    FOffsetY: TLightingOffset;
    FAngle: Double;
    FHeight: TLightingRange;
    FWidth: TLightingRange;
    procedure SetColor(const Value: TColor);
    procedure SetOffsetX(const Value: TLightingOffset);
    procedure SetOffsetY(const Value: TLightingOffset);
    procedure SetAlpha(const Value: TCnAlpha);
    procedure SetAngle(const Value: Double);
    procedure SetHeight(const Value: TLightingRange);
    procedure SetWidth(const Value: TLightingRange);
  public
    constructor Create; override;
    {* ¹¹ÔìÆ÷£¬ÓÃÓÚ´´½¨Ò»¸ö¸ÃÀàµÄÊµÀı}
    procedure Assign(Source: TPersistent); override;
    {* ¸³Öµ·½·¨£¬Ò»°ã²»ĞèÒªÊÖ¶¯µ÷ÓÃ}
  published
    property Alpha: TCnAlpha read FAlpha write SetAlpha default 180;
    {* ¹âÕÕĞ§¹ûµÄ²»Í¸Ã÷¶ÈÊôĞÔ}
    property Color: TColor read FColor write SetColor default clWhite;
    {* µÆ¹âÑÕÉ«}
    property OffsetX: TLightingOffset read FOffsetX write SetOffsetX default 0;
    {* ¹âÕÕÖĞĞÄµãÆ«ÒÆ·¶Î§£¨Ä¿±ê¾ØĞÎ¿í¶ÈµÄ°Ù·Ö±È£©£¬Îª¸º±íÊ¾×óÆ«£¬·¶Î§Îª -200..200}
    property OffsetY: TLightingOffset read FOffsetY write SetOffsetY default 0;
    {* ¹âÕÕÖĞĞÄµãÆ«ÒÆ·¶Î§£¨Ä¿±ê¾ØĞÎ¸ß¶ÈµÄ°Ù·Ö±È£©£¬Îª¸º±íÊ¾ÉÏÆ«£¬·¶Î§Îª -200..200}
    property Width: TLightingRange read FWidth write SetWidth default 80;
    {* ¹âÕÕ·¶Î§¿í¶È£¨Ä¿±ê¾ØĞÎ¿í¶ÈµÄ°Ù·Ö±È£©£¬·¶Î§Îª 0..1000}
    property Height: TLightingRange read FHeight write SetHeight default 80;
    {* ¹âÕÕ·¶Î§¸ß¶È£¨Ä¿±ê¾ØĞÎ¿í¶ÈµÄ°Ù·Ö±È£©£¬·¶Î§Îª 0..1000}
    property Angle: Double read FAngle write SetAngle;
    {* ¹âÕÕ·¶Î§½Ç¶È£¬·¶Î§Îª -360..360}
  end;

{ TCnFont }

  TCnFont = class(TFont)
  {* Æ½»¬ÌØĞ§×ÖÌåÀà£¬´Ó TFont ÅÉÉú¶øÀ´£¬Ìá¹©ÁËÒ»Ğ©ÌØĞ§ÏÔÊ¾²ÎÊı}
  private
    FOwner: TPersistent;
    FNoise: Byte;
    FAlpha: TCnAlpha;
    FGradient: TCnGradientColor;
    FShadow: TCnShadow;
    FTextureMode: TCnDrawMode;
    FStyleEx: TFontStyleExs;
    FTexture: TPicture;
    FQuality: TFontQuality;
    FScale: Integer;
    FUpdateCount: Integer;
    FLighting: TCnLighting;
    FSpray: Byte;
    function GetTexture: TPicture;
    procedure SetAlpha(const Value: TCnAlpha);
    procedure SetGradient(const Value: TCnGradientColor);
    procedure SetNoise(const Value: Byte);
    procedure SetShadow(const Value: TCnShadow);
    procedure SetStyleEx(const Value: TFontStyleExs);
    procedure SetTexture(const Value: TPicture);
    procedure SetTextureMode(const Value: TCnDrawMode);
    procedure SetLighting(const Value: TCnLighting);
    procedure SetSpray(const Value: Byte);
    procedure SetQuality(const Value: TFontQuality);
    procedure ChildChanged(Sender: TObject);
  protected
    function GetOwner: TPersistent; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Changed; override;
    property Owner: TPersistent read FOwner write FOwner;
    property UpdateCount: Integer read FUpdateCount;
    property Scale: Integer read FScale;
  public
    constructor Create;
    {* ¹¹ÔìÆ÷£¬ÓÃÓÚ´´½¨Ò»¸ö¸ÃÀàµÄÊµÀı}
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    {* ¸³Öµ·½·¨£¬ÔÊĞí´ÓTFontÖĞ¸³Öµ}
  published
    property StyleEx: TFontStyleExs read FStyleEx write SetStyleEx;
    {* À©Õ¹µÄ×ÖÌåÌØĞ§ÏÔÊ¾²ÎÊı£¬TFontStyleExs¼¯ºÏÀàĞÍ£¬Í¬Ê±¿ØÖÆÆäËüÒ»Ğ©²ÎÊıÊÇ·ñ¿ªÆô}
    property Quality: TFontQuality read FQuality write SetQuality default fqNormal;
    {* ×ÖÌåÆ½»¬ÏÔÊ¾¾«¶È}
    property Shadow: TCnShadow read FShadow write SetShadow;
    {* ×ÖÌåÒõÓ°ÏÔÊ¾²ÎÊı£¬ÊÜ StyleEx ÊôĞÔÓ°Ïì}
    property Gradient: TCnGradientColor read FGradient write SetGradient;
    {* ×ÖÌåÇ°¾°½¥±äĞ§¹û²ÎÊı£¬ÊÜ StyleEx ÊôĞÔÓ°Ïì}
    property Lighting: TCnLighting read FLighting write SetLighting;
    {* ×ÖÌåÇ°¾°¹âÕÕĞ§¹û²ÎÊı£¬ÊÜ StyleEx ÊôĞÔÓ°Ïì}
    property Texture: TPicture read GetTexture write SetTexture;
    {* ×ÖÌåÇ°¾°ÎÆÀíĞ§¹û²ÎÊı£¬ÊÜ StyleEx ÊôĞÔÓ°Ïì}
    property TextureMode: TCnDrawMode read FTextureMode write SetTextureMode
      default dmTiled;
    {* ×ÖÌåÇ°¾°ÎÆÀíĞ§¹ûÏÔÊ¾Ä£Ê½}
    property Alpha: TCnAlpha read FAlpha write SetAlpha default csMaxAlpha;
    {* ×ÖÌå²»Í¸Ã÷¶È²ÎÊı}
    property Noise: Byte read FNoise write SetNoise default 0;
    {* ×ÖÌåÇ°¾°Ëæ»úÔëÉùĞ§¹û²ÎÊı£¬ÊÜ StyleEx ÊôĞÔÓ°Ïì}
    property Spray: Byte read FSpray write SetSpray default 0;
    {* ×ÖÌåÇ°¾°Åç½¦Ğ§¹û²ÎÊı£¬ÊÜ StyleEx ÊôĞÔÓ°Ïì}
  end;

{ TCnFontMask }

  TCnFontMask = class
  private
    FBuff: PByteArray;
    FHeight: Integer;
    FWidth: Integer;
    FRowInc: Integer;
    function GetScanLine(Row: Integer): PByteArray;
    procedure SplitBlur(Amount: Integer);
  public
    destructor Destroy; override;
    procedure Outline;
    procedure SetSize(AWidth, AHeight: Integer);
    procedure CopyTo(Dst: TCnFontMask);
    procedure Blur(Amount: Integer);
    procedure Spray(Amount: Integer);
    property ScanLine[Row: Integer]: PByteArray read GetScanLine;
    property Buff: PByteArray read FBuff;
    property Height: Integer read FHeight;
    property Width: Integer read FWidth;
  end;

//--------------------------------------------------------//
// ¿ìËÙÍ¼Ïñ´¦ÀíÀà                                         //
//--------------------------------------------------------//

  TGdiAllocStyle = (gsInternal, gsNormal, gsActive);
  {* GDI ×ÊÔ´¹ÜÀí·½Ê½
   |<BR>
   |<BR>                  DC        HBITMAP
   |<BR>    gsInternal:  ¼´Ê±ÊÍ·Å  ¼´Ê±ÊÍ·Å
   |<BR>    gsNormal:    ¼´Ê±ÊÍ·Å  ¶¨Ê±ÊÍ·Å
   |<BR>    gsActive:    ¼´Ê±ÊÍ·Å  ´Ó²»ÊÍ·Å
  }

  TAdjustRange = -100..100;
  {* Í¼ÏñÊôĞÔ±ä»¯·¶Î§}

  TPenWeight = (pwThin, pwNormal, pwThick);
  {* Í¼Ïñ¿¹¾â³İ»­±Ê´ÖÏ¸³Ì¶È£¬¾ùÎª1¸öÏñËØ¿í¶ÈÒÔÄÚ
   |<BR>
   |<BR>    pwThin:    Ï¸»­±Ê£¬»æÖÆ³öµÄÍ¼ĞÎ½ÏÇ³
   |<BR>    pwNormal:  ³£¹æ»­±Ê£¬»æÖÆ³öµÄÍ¼ĞÎÊÊÖĞ
   |<BR>    pwThick:    ´Ö»­±Ê£¬»æÖÆ³öµÄÍ¼ĞÎ½Ï´Ö
  }

  TColorChannel = (ccRed, ccGreen, ccBlue);
  {* Í¼ÏñÑÕÉ«Í¨µÀ
   |<BR>
   |<BR>    ccRed:    ºìÉ«Í¨µÀ
   |<BR>    ccGreen:  ÂÌÉ«Í¨µÀ
   |<BR>    ccBlue:    À¶É«Í¨µÀ
  }
  TColorChannels = set of TColorChannel;
  {* Í¼ÏñÑÕÉ«Í¨µÀ¼¯ºÏ}

  TTurnAngle = (ta90, ta180, ta270);

const
  csAllChannels = [ccRed, ccGreen, ccBlue];

type
  TCnBitmap = class;

{ TCnCanvas }

  TCnCanvas = class(TCanvas)
  {* ÓÃÓÚ TCnBitmap ÄÚ²¿µÄ»­²¼Àà£¬ÇëÎğÖ±½ÓÊ¹ÓÃ}
  private
    FBitmap: TCnBitmap;
    FDC: HDC;
    procedure FreeContext;
    function GetHandle: HDC;
  protected
    procedure CreateHandle; override;
  public
    constructor Create(ABitmap: TCnBitmap);
    {* ¹¹ÔìÆ÷£¬ÓÃÓÚ´´½¨Ò»¸ö¸ÃÀàµÄÊµÀı}
    destructor Destroy; override;
    property Handle: HDC read GetHandle;
    {* »­²¼¾ä±ú£¬ÖØÉêÃ÷ÎªÖ»¶ÁÊôĞÔ}
  end;

{ TCnBitmap }

  TCnBitmap = class(TCnPersistent)
  {* CnPack ¿ìËÙÍ¼ÏñÀà}
  private
    FHandle: HBITMAP;
    FBitmapInfo: TBitmapInfo;
    FHeight: Integer;
    FWidth: Integer;
    FSize: Integer;
    FRowInc: Integer;
    FGap: Integer;
    FBits: Pointer;
    FScanLine: PPCnLines;
    FCanvas: TCnCanvas;
    FSmoothFilter: Boolean;
    FTransparentColor: TColor;
    FTransparent: Boolean;
    FGdiLastAccess: Cardinal;

    FGdiAllocStyle: TGdiAllocStyle;
    FPenPosF: TPointF;
    FPenWeight: TPenWeight;
    FPenColor: TColor;
    FFont: TCnFont;
    FGrayBmp: TBitmap;
    FFontMask: TCnFontMask;
    FFontClear: Boolean;
    FFontBkColor: TColor;

    class procedure OnGdiActTimer(Sender: TObject);
    function GetDC: HDC;
    function GetCanvas: TCnCanvas;
    procedure UpdateScanLine;
    function GetClientRect: TRect;
    function GetEmpty: Boolean;
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    function GetScanLine(Row: Integer): PCnLine;
    function GetPixel(x, y: Integer): TCnColor;
    procedure SetPixel(x, y: Integer; const Value: TCnColor);
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);

    procedure NormalResize(Dst: TCnBitmap);
    procedure SmoothResize(Dst: TCnBitmap);
    procedure SplitBlur(Amount: Integer);
    procedure SplitSharpen(Amount: Integer);
    function CheckAlphaSrc(Src: TCnBitmap; ARect: TRect;
      Stretch: Boolean): TCnBitmap;
    function GetPixelsF(x, y: Single): TCnColor;
    procedure SetPixelsF(x, y: Single; const Value: TCnColor);
    function CalcDrawRect(DstX, DstY: Integer; SrcRect, SrcClientRect:
      TRect; var dx, dy, sx, sy, w, h: Integer): Boolean;
    function ClipLineF(var X0, Y0, X1, Y1: Single; MinX, MaxX, MinY,
      MaxY: Single): Boolean;
    function GetResizeRect(Src: TRect): TRect;
    function GetFont: TCnFont;
    procedure SetFont(const Value: TCnFont);
    function GetHandle: HBITMAP;
    procedure InitGrayBmp;
    procedure FreeGrayBmp;
    procedure DrawFontMaskEx(const Text: string; Extend: TSize; Point: TPoint);
    procedure DrawFontMask(const Text: string);
    function GetShadowPoint: TPoint;
    function GetTextPoint: TPoint;
    procedure FontMaskBlend(x, y: Integer; AColor: TColor; Alpha: TCnAlpha;
      Mask: TCnFontMask);
    procedure FontMaskBlendEx(x, y: Integer; Alpha: TCnAlpha;
      Mask: TCnFontMask; ForeBmp: TCnBitmap);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure CleanUp; virtual;
    function GetTranColor: TCnColor;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoDraw(DstX, DstY: Integer; Src: TCnBitmap; SrcRect: TRect;
      Tran: Boolean);
    procedure DoSetPixelF(x, y: Integer; ARGB: TCnColor);
    function DoGetPixelF(x, y: Integer): TCnColor;

    function Compress(var pData: Pointer; ASize: Integer): Integer; virtual;
    function DeCompress(var pData: Pointer; ASize: Integer): Integer; virtual;
    function HandleAllocated: Boolean;
    procedure HandleNeeded; virtual;
    procedure HandleRelease(KeepData: Boolean = True); virtual;

    property BitmapInfo: TBitmapInfo read FBitmapInfo;
    property RowInc: Integer read FRowInc;
    property Gap: Integer read FGap;
  public
    function Equals(Obj: TObject): Boolean; {$IFDEF OBJECT_HAS_EQUAL} override; {$ENDIF}
    constructor Create; override;
    {* ¹¹ÔìÆ÷£¬ÓÃÓÚ´´½¨Ò»¸ö¸ÃÀàµÄÊµÀı}
    destructor Destroy; override;

    // ¸¨Öú¹¦ÄÜ
    procedure Fill(Color: TColor = clBlack);
    {* ÒÔÖ¸¶¨ÑÕÉ«Ìî³äÕû¸öÍ¼Ïñ£¬Ä¬ÈÏÎªºÚÉ«}
    procedure FillRect(Rect: TRect; Color: TColor = clBlack);
    {* ÒÔÖ¸¶¨ÑÕÉ«Ìî³äÕû¸öÒ»¸öÍ¼ÏñÇøÓò}
    procedure DrawLine(x1, y1, x2, y2: Integer; Color: TColor);
    {* ÒÔÖ¸¶¨ÑÕÉ«»æÖÆÒ»ÌõÖ±Ïß
     |<BR>
     |<BR> x1, y1: Integer    ÆğÊ¼µã×ø±ê
     |<BR> x2, y2: Integer    ½áÊøµã×ø±ê}
    procedure FrameRect(Rect: TRect; Color: TColor);
    {* ÒÔÖ¸¶¨ÑÕÉ«»æÖÆÒ»¸ö¾ØĞÎ¿ò¼Ü}
    procedure FreeImage; virtual;
    {* ½«Õû¸öÎ»Í¼Çå¿Õ£¬ÊÍ·ÅËùÓĞÒÑ·ÖÅäµÄ×ÊÔ´}
    function CreateRegion(var RgnData: PRgnData): Integer;
    {* ¸ù¾İµ±Ç°µÄÍ¸Ã÷É«ÊôĞÔ TransparentColor ´ÓÎ»Í¼ÖĞ´´½¨Ò»¸ö Region ÇøÓòÊı¾İ¡£
     |<BR> ±äÁ¿²ÎÊı RgnData Îª PRgnData Ö¸ÕëÀàĞÍ£¬ÓÃÓÚ½ÓÊÕÊı¾İ½á¹û¡£
     |<BR> ·µ»ØÖµÎª½á¹ûÊı¾İ¿éµÄ×Ö½ÚÊı¡£}

    // ÓëÍâ²¿½»»»Êı¾İ
    procedure Assign(Source: TPersistent); override;
    {* ¸³Öµ¹ı³Ì£¬ÔÊĞí´ÓÆäËüÍ¼Ïñ¶ÔÏóÖĞ»ñÈ¡Êı¾İ¡£
     |<BR>
     |<BR> Ö§³ÖÒÔÏÂµÄÔ´Í¼ÏñÀàĞÍ£º
     |<BR> TCnBitmap¡¢TBitmap¡¢TGraphic¡¢TPicture ÒÔ¼°ËüÃÇµÄÅÉÉúÀàÈç TIcon¡¢TJpegImage µÈ
     |<BR> µ±SourceÎªnilÊ±£¬Çå¿Õµ±Ç°Î»Í¼£¬ÊÍ·ÅËùÓĞÒÑ·ÖÅäµÄ×ÊÔ´¡£}
    procedure SetSize(AWidth, AHeight: Integer); overload;
    {* ÉèÖÃµ±Ç°Í¼Ïñ´óĞ¡£¬Èç¹û±ä¸ü´óĞ¡£¬Ô­Í¼ÏñÊı¾İ½«¶ªÊ§¡£}
    procedure SetSize(ARect: TRect); overload;
    {* ÉèÖÃµ±Ç°Í¼Ïñ´óĞ¡£¬Èç¹û±ä¸ü´óĞ¡£¬Ô­Í¼ÏñÊı¾İ½«¶ªÊ§}
    procedure LoadBlank(AWidth, AHeight: Integer);
    {* ÉèÖÃµ±Ç°Í¼Ïñ´óĞ¡£¬¹¦ÄÜÍ¬SetSize¡£}
    procedure LoadFromMemory(ABits: Pointer; AWidth, AHeight: Integer);
    {* ´ÓÄÚ´æÖĞ×°ÔØÎ»Í¼
     |<BR>
     |<BR> ABits: Pointer    Ö¸ÏòÒ»¿éÊı¾İÇø£¬Êı¾İÄÚÈİÓ¦¸ÃÊÇ 24 Î»¸ñÊ½µÄÍ¼ÏñÊı¾İ
     |<BR> AWidth, AHeight: Integer    Î»Í¼µÄ¿í¶ÈºÍ¸ß¶È}
    procedure LoadFromStream(Stream: TStream);
    {* ´ÓÁ÷ÖĞ×°ÔØÎ»Í¼}
    procedure LoadFromFile(const FileName: string);
    {* ´ÓÍ¼ÏñÎÄ¼şÖĞ×°ÔØÎ»Í¼
     |<BR> ÄÚ²¿Ê¹ÓÃTPictureÀ´¶ÁÈ¡ÎÄ¼ş£¬ÔÊĞíÏµÍ³ÖĞÖ§³ÖµÄÍ¼ÏñÎÄ¼ş¸ñÊ½¡£
     |<BR> Èç Icon¡¢Wmf¡¢Jpeg£¨ĞèÒª Jpeg µ¥Ôª£©µÈÍ¼ÏñÎÄ¼ş}
    procedure LoadFromResourceName(instance: THandle; const ResName: string);
    {* ´Ó×ÊÔ´ÖĞ×°ÔØÎ»Í¼£¬²ÎÊıÎªÄ£¿é¾ä±úºÍ BITMAP ×ÊÔ´Ãû}
    procedure LoadFromResourceID(instance: THandle; ResID: Integer);
    {* ´Ó×ÊÔ´ÖĞ×°ÔØÎ»Í¼£¬²ÎÊıÎªÄ£¿é¾ä±úºÍ BITMAP ×ÊÔ´ ID}
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
      APalette: HPALETTE);
    {* ´Ó¼ôÌû°åÖĞ×°ÔØÎ»Í¼}
    procedure SaveToStream(Stream: TStream);
    {* ½«µ±Ç°Î»Í¼±£´æµ½Á÷}
    procedure SaveToFile(const FileName: string);
    {* ½«µ±Ç°Î»Í¼±£´æµ½ Bmp ÎÄ¼ş£¬ÎÄ¼ş¸ñÊ½Îª 24 Î» Bmp Î»Í¼ÎÄ¼ş}
    procedure SaveToClipboardFormat(var Format: Word; var Data: THandle;
      var APalette: HPALETTE);
    {* ¸´ÖÆÎ»Í¼µ½¼ôÌû°åÖĞ}

    // Í¼Ïñ»æÖÆ·½·¨
    procedure Draw(DstX, DstY: Integer; Src: TCnBitmap); overload;
    {* Í¼Ïñ»æÖÆ·½·¨£¬½«Ô´Í¼ÏñÈ«²¿»æÖÆµ½µ±Ç°Î»Í¼ÖĞ
     |<BR>
     |<BR> DstX, DstY: Integer    µ±Ç°Í¼ÏñµÄ×óÉÏ½Ç×ø±ê
     |<BR> Src: TCnBitmap    Ô´Í¼Ïñ}
    procedure DrawEx(DstX, DstY: Integer; Src: TCnBitmap; SrcRect: TRect); overload;
    {* ÔöÇ¿µÄÍ¼Ïñ»æÖÆ·½·¨£¬½«Ô´Í¼ÏñÖĞµÄÒ»²¿·Ö»æÖÆµ½µ±Ç°Î»Í¼ÖĞ
     |<BR>
     |<BR> DstX, DstY: Integer    µ±Ç°Í¼ÏñµÄ×óÉÏ½Ç×ø±ê
     |<BR> Src: TCnBitmap    Ô´Í¼Ïñ
     |<BR> SrcRect: TRect    Ô´Í¼Ïñ¾ØĞÎ}
    procedure Draw(DstX, DstY: Integer; Src: TGraphic); overload;
    {* Í¼Ïñ»æÖÆ·½·¨£¬½«Ô´Í¼ÏñÈ«²¿»æÖÆµ½µ±Ç°Î»Í¼ÖĞ
     |<BR>
     |<BR> DstX, DstY: Integer    µ±Ç°Í¼ÏñµÄ×óÉÏ½Ç×ø±ê
     |<BR> Src: TGraphic    Ô´Í¼Ïñ£¬¿ÉÒÔÊÇTIcon¡¢TBitmap¡¢TJpegImageµÈÅÉÉúÀà}
    procedure DrawEx(DstX, DstY: Integer; Src: TGraphic; SrcRect: TRect); overload;
    {* ÔöÇ¿µÄÍ¼Ïñ»æÖÆ·½·¨£¬½«Ô´Í¼ÏñÖĞµÄÒ»²¿·Ö»æÖÆµ½µ±Ç°Î»Í¼ÖĞ
     |<BR>
     |<BR> DstX, DstY: Integer    µ±Ç°Í¼ÏñµÄ×óÉÏ½Ç×ø±ê
     |<BR> Src: TGraphic     Ô´Í¼Ïñ£¬¿ÉÒÔÊÇ TIcon¡¢TBitmap¡¢TJpegImage µÈÅÉÉúÀà
     |<BR> SrcRect: TRect    Ô´Í¼Ïñ¾ØĞÎ}
    procedure Draw(DstX, DstY: Integer; hSrc: HDC; SrcRect: TRect); overload;
    {* Í¼Ïñ»æÖÆ·½·¨£¬½«Ô´DCÉÏµÄÒ»²¿·Ö»æÖÆµ½µ±Ç°Î»Í¼ÖĞ
     |<BR>
     |<BR> DstX, DstY: Integer    Îªµ±Ç°Í¼ÏñµÄ×óÉÏ½Ç×ø±ê
     |<BR> hSrc: HDC         Ô´ DC ¾ä±ú£¬¿ÉÒÔÊÇ TCanvas.Handle
     |<BR> SrcRect: TRect    Ô´Í¼Ïñ¾ØĞÎ}
    procedure DrawTo(hDst: HDC; DstX, DstY: Integer); overload;
    {* Í¼Ïñ»æÖÆ·½·¨£¬½«µ±Ç°Î»Í¼È«²¿»æÖÆµ½Ä¿±ê DC ÉÏ
     |<BR>
     |<BR> hDst: HDC        Ä¿±ê DC ¾ä±ú£¬¿ÉÒÔÊÇ TCanvas.Handle
     |<BR> DstX, DstY: Integer    Ä¿±ê»­²¼µÄ×óÉÏ½Ç×ø±ê}
    procedure DrawToEx(hDst: HDC; DstX, DstY: Integer; SrcRect: TRect); overload;
    {* ÔöÇ¿µÄÍ¼Ïñ»æÖÆ·½·¨£¬½«µ±Ç°µÄÒ»²¿·Ö»æÖÆµ½Ä¿±ê DC ÉÏ
     |<BR>
     |<BR> hDst: HDC        Ä¿±ê DC ¾ä±ú£¬¿ÉÒÔÊÇ TCanvas.Handle
     |<BR> DstX, DstY: Integer    Ä¿±ê»­²¼µÄ×óÉÏ½Ç×ø±ê
     |<BR> SrcRect: TRect    µ±Ç°Í¼ÏñÔ´¾ØĞÎ}
    procedure DrawMode(Src: TCnBitmap; Mode: TCnDrawMode); overload;
    {* Ö¸¶¨Ä£Ê½µÄÍ¼Ïñ»æÖÆ·½·¨£¬½«Ô´Í¼Ïñ°´Ö¸¶¨µÄÄ£Ê½»æÖÆµ½µ±Ç°Í¼ÏñÉÏ
     |<BR>
     |<BR> Src: TCnBitmap     Ô´Í¼Ïñ
     |<BR> Mode: TCnDrawMode  »æÖÆ·½Ê½}
    procedure DrawModeEx(Src: TCnBitmap; Mode: TCnDrawMode; Alpha: TCnAlpha); overload;
    {* Ö¸¶¨Ä£Ê½µÄÍ¼Ïñ»æÖÆ·½·¨£¬½«Ô´Í¼Ïñ°´Ö¸¶¨µÄÄ£Ê½»æÖÆµ½µ±Ç°Í¼ÏñÉÏ£¬Ö§³Ö Alpha »ìºÏ
     |<BR>
     |<BR> Src: TCnBitmap     Ô´Í¼Ïñ
     |<BR> Mode: TCnDrawMode  »æÖÆ·½Ê½}
    procedure DrawMode(Src: TGraphic; Mode: TCnDrawMode); overload;
    {* Ö¸¶¨Ä£Ê½µÄÍ¼Ïñ»æÖÆ·½·¨£¬½«Ô´Í¼Ïñ°´Ö¸¶¨µÄÄ£Ê½»æÖÆµ½µ±Ç°Í¼ÏñÉÏ
     |<BR>
     |<BR> Src: TGraphic      Ô´Í¼Ïñ£¬¿ÉÒÔÊÇTIcon¡¢TBitmap¡¢TJpegImageµÈÅÉÉúÀà
     |<BR> Mode: TCnDrawMode  »æÖÆ·½Ê½}
    procedure DrawModeEx(Src: TGraphic; Mode: TCnDrawMode; Alpha: TCnAlpha); overload;
    {* Ö¸¶¨Ä£Ê½µÄÍ¼Ïñ»æÖÆ·½·¨£¬½«Ô´Í¼Ïñ°´Ö¸¶¨µÄÄ£Ê½»æÖÆµ½µ±Ç°Í¼ÏñÉÏ£¬Ö§³Ö Alpha »ìºÏ
     |<BR>
     |<BR> Src: TGraphic      Ô´Í¼Ïñ£¬¿ÉÒÔÊÇTIcon¡¢TBitmap¡¢TJpegImageµÈÅÉÉúÀà
     |<BR> Mode: TCnDrawMode  »æÖÆ·½Ê½}

    // ÖĞĞÄ»æÖÆ
    procedure CenterDraw(Src: TCnBitmap); overload;
    {* ½«Ô´Í¼Ïñ»æÖÆµ½µ±Ç°Í¼ÏñµÄÖĞĞÄÎ»ÖÃÉÏ
     |<BR>
     |<BR> Src: TCnBitmap      Ô´Í¼Ïñ}
    procedure CenterDraw(Src: TGraphic); overload;
    {* ½«Ô´Í¼Ïñ»æÖÆµ½µ±Ç°Í¼ÏñµÄÖĞĞÄÎ»ÖÃÉÏ
     |<BR>
     |<BR> Src: TGraphic      Ô´Í¼Ïñ£¬¿ÉÒÔÊÇ TIcon¡¢TBitmap¡¢TJpegImage µÈÅÉÉúÀà}

    // Æ½ÆÌ»æÖÆ
    procedure TileDraw(Src: TCnBitmap); overload;
    {* ½«Ô´Í¼ÏñÆ½ÆÌ»æÖÆµ½µ±Ç°Í¼Ïñ
     |<BR>
     |<BR> Src: TCnBitmap      Ô´Í¼Ïñ}
    procedure TileDrawEx(DstRect: TRect; Src: TCnBitmap); overload;
    {* ½«Ô´Í¼ÏñÆ½ÆÌ»æÖÆµ½µ±Ç°Í¼ÏñÖ¸¶¨ÇøÓòÄÚ
     |<BR>
     |<BR> DstRect: TRect      µ±Ç°Í¼ÏñÄ¿±ê¾ØĞÎ
     |<BR> Src: TCnBitmap      Ô´Í¼Ïñ}
    procedure TileDraw(Src: TGraphic); overload;
    {* ½«Ô´Í¼ÏñÆ½ÆÌ»æÖÆµ½µ±Ç°Í¼Ïñ
     |<BR>
     |<BR> Src: TGraphic      Ô´Í¼Ïñ£¬¿ÉÒÔÊÇ TIcon¡¢TBitmap¡¢TJpegImage µÈÅÉÉúÀà}
    procedure TileDrawEx(DstRect: TRect; Src: TGraphic); overload;
    {* ½«Ô´Í¼ÏñÆ½ÆÌ»æÖÆµ½µ±Ç°Í¼ÏñÖ¸¶¨ÇøÓòÄÚ
     |<BR>
     |<BR> DstRect: TRect      µ±Ç°Í¼ÏñÄ¿±ê¾ØĞÎ
     |<BR> Src: TCnBitmap      Ô´Í¼Ïñ£¬¿ÉÒÔÊÇ TIcon¡¢TBitmap¡¢TJpegImage µÈÅÉÉúÀà}
    procedure TileDrawTo(hDst: HDC; DstRect: TRect); overload;
    {* ½«µ±Ç°Í¼ÏñÆ½ÆÌ»æÖÆµ½Ä¿±êDCÖ¸¶¨ÇøÓòÄÚ
     |<BR>
     |<BR> hDst: HDC          Ä¿±ê DC ¾ä±ú£¬¿ÉÒÔÊÇTCanvas.Handle
     |<BR> DstRect: TRect     Ä¿±ê¾ØĞÎ}

    // Ëõ·Å»æÖÆ
    procedure StretchDraw(Src: TCnBitmap); overload;
    {* ½«Ô´Í¼ÏñËõ·Å»æÖÆµ½µ±Ç°Í¼ÏñÖĞ
     |<BR>
     |<BR> Src: TCnBitmap    Ô´Í¼Ïñ}
    procedure StretchDrawEx(DstRect, SrcRect: TRect; Src: TCnBitmap); overload;
    {* ½«Ô´Í¼ÏñµÄÒ»²¿·ÖËõ·Å»æÖÆµ½µ±Ç°Í¼ÏñÖĞÖ¸¶¨ÇøÓòÄÚ
     |<BR>
     |<BR> DstRect: TRect    Ä¿±ê¾ØĞÎ
     |<BR> SrcRect: TRect    Ô´¾ØĞÎ
     |<BR> Src: TCnBitmap    Ô´Í¼Ïñ}
    procedure StretchDraw(Src: TGraphic); overload;
    {* ½«Ô´Í¼ÏñËõ·Å»æÖÆµ½µ±Ç°Í¼ÏñÖĞ
     |<BR>
     |<BR> Src: TGraphic     Ô´Í¼Ïñ£¬¿ÉÒÔÊÇ TIcon¡¢TBitmap¡¢TJpegImage µÈÅÉÉúÀà}
    procedure StretchDrawEx(DstRect, SrcRect: TRect; Src: TGraphic); overload;
    {* ½«Ô´Í¼ÏñµÄÒ»²¿·ÖËõ·Å»æÖÆµ½µ±Ç°Í¼ÏñÖ¸¶¨ÇøÓòÄÚ
     |<BR>
     |<BR> DstRect: TRect     Ä¿±ê¾ØĞÎ
     |<BR> SrcRect: TRect     Ô´¾ØĞÎ
     |<BR> Src: TGraphic      Ô´Í¼Ïñ£¬¿ÉÒÔÊÇ TIcon¡¢TBitmap¡¢TJpegImage µÈÅÉÉúÀà}
    procedure StretchDraw(SrcRect: TRect; hSrc: HDC); overload;
    {* ½«Ô´DCÉÏµÄÖ¸¶¨ÇøÓòËõ·Å»æÖÆµ½µ±Ç°Í¼ÏñÖĞ
     |<BR>
     |<BR> SrcRect: TRect     Ô´¾ØĞÎ
     |<BR> hSrc: HDC          Ô´ DC ¾ä±ú£¬¿ÉÒÔÊÇ TCanvas.Handle}
    procedure StretchDrawEx(DstRect, SrcRect: TRect; hSrc: HDC); overload;
    {* ½«Ô´DCÉÏµÄÖ¸¶¨ÇøÓòËõ·Å»æÖÆµ½µ±Ç°Í¼ÏñÖ¸¶¨ÇøÓòÄÚ
     |<BR>
     |<BR> DstRect: TRect     Ä¿±ê¾ØĞÎ
     |<BR> SrcRect: TRect     Ô´¾ØĞÎ
     |<BR> hSrc: HDC          Ô´ DC ¾ä±ú£¬¿ÉÒÔÊÇ TCanvas.Handle}
    procedure StretchDrawTo(Dst: TImage); overload;
    {* ½«µ±Ç°Í¼ÏñËõ·Å»æÖÆµ½TImage¿Ø¼şÖĞ
     |<BR>
     |<BR> Dst: TImage       Ä¿±ê¿Ø¼ş}
    procedure StretchDrawTo(hDst: HDC; DstRect: TRect); overload;
    {* ½«µ±Ç°Í¼ÏñËõ·Å»æÖÆµ½DCÖĞ
     |<BR>
     |<BR> hDst: HDC         Ä¿±ê DC ¾ä±ú£¬¿ÉÒÔÊÇ TCanvas.Handle
     |<BR> DstRect: TRect    Ä¿±ê¾ØĞÎ}
    procedure StretchDrawToEx(hDst: HDC; DstRect, SrcRect: TRect); overload;
    {* ½«µ±Ç°Í¼ÏñµÄÒ»²¿·ÖËõ·Å»æÖÆµ½DCÖĞ
     |<BR>
     |<BR> hDst: HDC         Ä¿±êDC¾ä±ú£¬¿ÉÒÔÊÇ TCanvas.Handle
     |<BR> DstRect: TRect    Ä¿±ê¾ØĞÎ
     |<BR> SrcRect: TRect    Ô´¾ØĞÎ}

    // Alpha»ìºÏ»æÖÆ
    procedure AlphaDraw(Src: TCnBitmap; Alpha: TCnAlpha; Stretch: Boolean); overload;
    {* ½«Ô´Í¼ÏñÓëµ±Ç°Í¼Ïñ°´Ö¸¶¨µÄ±ÈÀı»ìºÏµ½µ±Ç°Í¼ÏñÖĞ
     |<BR>
     |<BR> Src: TCnBitmap     Ô´Í¼Ïñ
     |<BR> Alpha: TCnAlpha    Ô´Í¼ÏñµÄ²»Í¸Ã÷¶È
     |<BR> Stretch: Boolean   µ±Í¼Ïñ´óĞ¡²»Ò»ÖÂÊ±£¬ÊÇ·ñ×Ô¶¯¶ÔÔ´Í¼Ïñ½øĞĞËõ·Å}
    procedure AlphaDraw(DstX, DstY: Integer; Src: TCnBitmap; SrcRect: TRect;
      Alpha: TCnAlpha); overload;
    {* ½«Ô´Í¼ÏñÖĞµÄÒ»²¿·ÖÓëµ±Ç°Í¼Ïñ°´Ö¸¶¨µÄ±ÈÀı»ìºÏµ½µ±Ç°Í¼ÏñÖ¸¶¨Î»ÖÃÖĞ
     |<BR>
     |<BR> DstX, DstY: Integer    Ä¿±êÎ»ÖÃ×óÉÏ½Ç×ø±ê
     |<BR> Src: TCnBitmap         Ô´Í¼Ïñ
     |<BR> SrcRect: TRect         Ô´¾ØĞÎ
     |<BR> Alpha: TCnAlpha        Ô´Í¼ÏñµÄ²»Í¸Ã÷¶È}
    procedure AlphaDrawGrad(Src: TCnBitmap; Style: TCnGradStyle;
      Stretch: Boolean; StartAlpha: TCnAlpha = 0; EndAlpha: TCnAlpha = csMaxAlpha);
    {* ½«Ô´Í¼ÏñÓëµ±Ç°Í¼Ïñ°´½¥±äµÄ±ÈÀı»ìºÏµ½µ±Ç°Í¼ÏñÖ¸¶¨Î»ÖÃÖĞ
     |<BR>
     |<BR> Src: TCnBitmap         Ô´Í¼Ïñ
     |<BR> Style: TCnGradStyle    ½¥±ä»ìºÏ·½Ê½
     |<BR> Stretch: Boolean       µ±Í¼Ïñ´óĞ¡²»Ò»ÖÂÊ±£¬ÊÇ·ñ×Ô¶¯¶ÔÔ´Í¼Ïñ½øĞĞËõ·Å
     |<BR> StartAlpha: TCnAlpha   ½¥±äÆğÊ¼µÄ²»Í¸Ã÷¶È
     |<BR> EndAlpha: TCnAlpha     ½¥±ä½áÊøµÄ²»Í¸Ã÷¶È}
    procedure AlphaDrawEx(DstRect: TRect; Front, Back: TCnBitmap; Alpha: TCnAlpha;
      Stretch: Boolean);
    {* ½«Á½¸öÍ¼Ïñ°´Ö¸¶¨µÄ±ÈÀı»ìºÏµ½µ±Ç°Í¼ÏñÖ¸¶¨ÇøÓòÖĞ
     |<BR>
     |<BR> Front: TCnBitmap       Ç°¾°Í¼Ïñ
     |<BR> Back: TCnBitmap        ±³¾°Í¼Ïñ
     |<BR> Alpha: TCnAlpha        Ç°¾°Í¼ÏñµÄ²»Í¸Ã÷¶È
     |<BR> Stretch: Boolean       µ±Í¼Ïñ´óĞ¡²»Ò»ÖÂÊ±£¬ÊÇ·ñ×Ô¶¯¶ÔÔ´Í¼Ïñ½øĞĞËõ·Å}

    // ½¥±äÉ«»æÖÆ
    procedure DrawGradient(GradColor: TCnGradientColor);
    {* ÔÚµ±Ç°Í¼ÏñÖĞ²úú½¥±äÑÕÉ«Ğ§¹û
     |<BR>
     |<BR> GradColor: TCnGradientColor    ½¥±äĞ§¹û²ÎÊı}
    procedure DrawGradientEx(GradColor: TCnGradientColor; Rect: TRect; Alpha:
      TCnAlpha);
    {* ÔÚµ±Ç°Í¼ÏñÖ¸¶¨ÇøÓòÖĞ²úÉú°ëÍ¸Ã÷µÄ½¥±äÑÕÉ«Ğ§¹û
     |<BR>
     |<BR> GradColor: TCnGradientColor    ½¥±äĞ§¹û²ÎÊı
     |<BR> Rect: TRect        Ö¸¶¨ÇøÓò
     |<BR> Alpha: TCnAlpha    ½¥±äĞ§¹ûµÄ²»Í¸Ã÷¶È
     |<BR> ×¢£ºµ±Ñ¡Ôñ·øÉä½¥±ä·½Ê½Ê±£¬¸ù¾İµ±Ç° SmoothFilter ÊôĞÔ¿ÉÖ§³Ö¿¹¾â³İ´¦Àí }

    // °´Å¥Î»Í¼»æÖÆ
    procedure Disabled;
    {* ½«µ±Ç°Í¼Ïñ°´Ê§Ğ§°´Å¥µÄ·ç¸ñ½øĞĞ»æÖÆ£¬¸ù¾İµ±Ç°µÄÍ¸Ã÷É«ÊôĞÔÅĞ¶Ï}
    procedure DisabledEx(OutlineColor, BackColor, HighlightColor,
      ShadowColor: TColor; DrawHighlight: Boolean);
    {* ½«µ±Ç°Í¼Ïñ°´Ê§Ğ§°´Å¥µÄ·ç¸ñ½øĞĞ»æÖÆ£¬¸ù¾İµ±Ç°µÄÍ¸Ã÷É«ÊôĞÔÅĞ¶Ï
     |<BR>
     |<BR> OutlineColor: TColor    Ä¿±êÍ¼ÏñÂÖÀªÑÕÉ«
     |<BR> BackColor: TColor       Ä¿±êÍ¼Ïñ±³¾°ÑÕÉ«
     |<BR> HighlightColor: TColor  Ä¿±êÍ¼Ïñ¸ßÁÁÇøÑÕÉ«
     |<BR> ShadowColor: TColor     Ä¿±êÍ¼ÏñÒõÓ°ÑÕÉ«
     |<BR> DrawHighlight: Boolean  ÊÇ·ñ»æÖÆ¸ßÁÁÇø}
    procedure DrawDisabled(hDst: HDC; ARect: TRect);
    {* ½«µ±Ç°Í¼Ïñ°´Ê§Ğ§°´Å¥µÄ·ç¸ñ»æÖÆµ½Ä¿±êDCÉÏ£¬¸ù¾İµ±Ç°µÄÍ¸Ã÷É«ÊôĞÔÅĞ¶Ï
       Íê³É»æÖÆºóµ±Ç°Í¼ÏñÄÚÈİ²»±ä
     |<BR>
     |<BR> hDst: HDC         Ä¿±ê DC ¾ä±ú£¬¿ÉÒÔÊÇ TCanvas.Handle
     |<BR> ARect: TRect      Ä¿±ê¾ØĞÎ}
    procedure DrawDisabledEx(hDst: HDC; ARect: TRect; OutlineColor,
      BackColor, HighlightColor, ShadowColor: TColor; DrawHighlight: Boolean);
    {* ½«µ±Ç°Í¼Ïñ°´Ê§Ğ§°´Å¥µÄ·ç¸ñ»æÖÆµ½Ä¿±ê DC ÉÏ£¬¸ù¾İµ±Ç°µÄÍ¸Ã÷É«ÊôĞÔÅĞ¶Ï
       Íê³É»æÖÆºóµ±Ç°Í¼ÏñÄÚÈİ²»±ä
     |<BR>
     |<BR> hDst: HDC               Ä¿±ê DC ¾ä±ú£¬¿ÉÒÔÊÇ TCanvas.Handle
     |<BR> ARect: TRect            Ä¿±ê¾ØĞÎ
     |<BR> OutlineColor: TColor    Ä¿±êÍ¼ÏñÂÖÀªÑÕÉ«
     |<BR> BackColor: TColor       Ä¿±êÍ¼Ïñ±³¾°ÑÕÉ«
     |<BR> HighlightColor: TColor  Ä¿±êÍ¼Ïñ¸ßÁÁÇøÑÕÉ«
     |<BR> ShadowColor: TColor     Ä¿±êÍ¼ÏñÒõÓ°ÑÕÉ«
     |<BR> DrawHighlight: Boolean  ÊÇ·ñ»æÖÆ¸ßÁÁÇø}
    procedure Shadowed;
    {* ½«µ±Ç°Í¼Ïñ°´´øÒõÓ°°´Å¥µÄ·ç¸ñ½øĞĞ»æÖÆ£¬¸ù¾İµ±Ç°µÄÍ¸Ã÷É«ÊôĞÔÅĞ¶Ï}
    procedure ShadowedEx(OutlineColor, ShadowColor, BackColor: TColor;
      Blur: Boolean; OffsetX, OffsetY: Integer);
    {* ½«µ±Ç°Í¼Ïñ°´´øÒõÓ°°´Å¥µÄ·ç¸ñ½øĞĞ»æÖÆ£¬¸ù¾İµ±Ç°µÄÍ¸Ã÷É«ÊôĞÔÅĞ¶Ï
     |<BR>
     |<BR> OutlineColor: TColor    Ä¿±êÍ¼ÏñÂÖÀªÑÕÉ«
     |<BR> ShadowColor: TColor     Ä¿±êÍ¼ÏñÒõÓ°ÑÕÉ«
     |<BR> BackColor: TColor       Ä¿±êÍ¼Ïñ±³¾°ÑÕÉ«
     |<BR> Blur: Boolean           ÒõÓ°ÊÇ·ñÄ£ºı
     |<BR> OffsetX: Integer        ÒõÓ°Ë®Æ½Æ«ÒÆÁ¿£¬Îª¸º±íÊ¾×óÆ«
     |<BR> OffsetY: Integer        ÒõÓ°´¹Ö±Æ«ÒÆÁ¿£¬Îª¸º±íÊ¾ÉÏÆ«}
    procedure DrawShadowed(hDst: HDC; ARect: TRect);
    {* ½«µ±Ç°Í¼Ïñ°´´øÒõÓ°°´Å¥µÄ·ç¸ñ»æÖÆµ½Ä¿±êDCÉÏ£¬¸ù¾İµ±Ç°µÄÍ¸Ã÷É«ÊôĞÔÅĞ¶Ï
       Íê³É»æÖÆºóµ±Ç°Í¼ÏñÄÚÈİ²»±ä
     |<BR>
     |<BR> hDst: HDC         Ä¿±ê DC ¾ä±ú£¬¿ÉÒÔÊÇ TCanvas.Handle
     |<BR> ARect: TRect      Ä¿±ê¾ØĞÎ}
    procedure DrawShadowedEx(hDst: HDC; ARect: TRect; OutlineColor, ShadowColor,
      BackColor: TColor; Blur: Boolean; OffsetX, OffsetY: Integer);
    {* ½«µ±Ç°Í¼Ïñ°´´øÒõÓ°°´Å¥µÄ·ç¸ñ»æÖÆµ½Ä¿±êDCÉÏ£¬¸ù¾İµ±Ç°µÄÍ¸Ã÷É«ÊôĞÔÅĞ¶Ï
       Íê³É»æÖÆºóµ±Ç°Í¼ÏñÄÚÈİ²»±ä
     |<BR>
     |<BR> hDst: HDC               Ä¿±ê DC ¾ä±ú£¬¿ÉÒÔÊÇ TCanvas.Handle
     |<BR> ARect: TRect            Ä¿±ê¾ØĞÎ
     |<BR> OutlineColor: TColor    Ä¿±êÍ¼ÏñÂÖÀªÑÕÉ«
     |<BR> ShadowColor: TColor     Ä¿±êÍ¼ÏñÒõÓ°ÑÕÉ«
     |<BR> BackColor: TColor       Ä¿±êÍ¼Ïñ±³¾°ÑÕÉ«
     |<BR> Blur: Boolean           ÒõÓ°ÊÇ·ñÄ£ºı
     |<BR> OffsetX: Integer        ÒõÓ°Ë®Æ½Æ«ÒÆÁ¿£¬Îª¸º±íÊ¾×óÆ«
     |<BR> OffsetY: Integer        ÒõÓ°´¹Ö±Æ«ÒÆÁ¿£¬Îª¸º±íÊ¾ÉÏÆ«}

    // Í¼ÏñÑÕÉ«ÊôĞÔµ÷Õû·½·¨
    procedure RGB(ra, ga, ba: TAdjustRange);
    {* µ÷Õûµ±Ç°Í¼ÏñµÄ¸÷ÑÕÉ«·ÖÁ¿
     |<BR>
     |<BR> ra, ga, ba: TAdjustRange  ·Ö±ğÎªºì¡¢ÂÌ¡¢À¶·ÖÁ¿µ÷Õû·¶Î§
     |<BR> ·¶Î§ÖµÎª -100..100£¬0 ±íÊ¾²»±ä£¬ÕıÎªÔö¼Ó£¬¸ºÎª¼õÉÙ}
    procedure Brightness(Range: TAdjustRange; Channels: TColorChannels =
      csAllChannels);
    {* µ÷Õûµ±Ç°Í¼ÏñµÄÁÁ¶È
     |<BR>
     |<BR> Range: TAdjustRange  ÁÁ¶È·¶Î§ÖµÎª -100..100£¬0 ±íÊ¾²»±ä£¬ÕıÎªÔö¼Ó£¬¸ºÎª¼õÉÙ
     |<BR> Channels: TColorChannels    ÑÕÉ«Í¨µÀÉèÖÃ}
    procedure Contrast(Range: TAdjustRange; Channels: TColorChannels = csAllChannels);
    {* µ÷Õûµ±Ç°Í¼ÏñµÄ¶Ô±È¶È
     |<BR>
     |<BR> Range: TAdjustRange  ±È¶Ô¶È·¶Î§ÖµÎª -100..100£¬0 ±íÊ¾²»±ä£¬ÕıÎªÔö¼Ó£¬¸ºÎª¼õÉÙ
     |<BR> Channels: TColorChannels    ÑÕÉ«Í¨µÀÉèÖÃ}
    procedure Saturation(Range: TAdjustRange; Channels: TColorChannels =
      csAllChannels);
    {* µ÷Õûµ±Ç°Í¼ÏñµÄÑÕÉ«±¥ºÍ¶È
     |<BR>
     |<BR> Range: TAdjustRange  ±¥ºÍ¶È·¶Î§ÖµÎª -100..100£¬0 ±íÊ¾²»±ä£¬ÕıÎªÔö¼Ó£¬¸ºÎª¼õÉÙ
     |<BR> Channels: TColorChannels    ÑÕÉ«Í¨µÀÉèÖÃ}
    procedure Levels(InLow, InHigh, OutLow, OutHigh: Byte;
      Channels: TColorChannels = csAllChannels);
    {* µ÷Õûµ±Ç°Í¼ÏñµÄÉ«½×
     |<BR>
     |<BR> InLow, InHigh: Byte    ÊäÈëÍ¼ÏñµÄ»Ò¶ÈÖµÉÏ¡¢ÏÂÏŞ
     |<BR> OutLow, OutHigh: Byte  Êä³öÍ¼ÏñµÄ»Ò¶ÈÖµÉÏ¡¢ÏÂÏŞ
     |<BR> Channels: TColorChannels    ÑÕÉ«Í¨µÀÉèÖÃ}
    procedure Grayscale(Channels: TColorChannels = csAllChannels);
    {* ½«µ±Ç°Í¼Ïñ×ª»»Îª»Ò¶ÈÍ¼
     |<BR>
     |<BR> Channels: TColorChannels    ÑÕÉ«Í¨µÀÉèÖÃ}
    procedure Invert(Channels: TColorChannels = csAllChannels);
    {* ½«µ±Ç°Í¼ÏñËùÓĞÏñËØµÄÑÕÉ«·´×ª
     |<BR>
     |<BR> Channels: TColorChannels    ÑÕÉ«Í¨µÀÉèÖÃ}
    procedure Colorize(Color: TColor); overload;
    {* µ÷Õûµ±Ç°Í¼Ïñ°´Ö¸¶¨ÑÕÉ«²ÊÉ«»¯£¬Ö§³ÖÍ¼ÏñÍ¸Ã÷ÉèÖÃ
     |<BR>
     |<BR> Color: TColor  Ö¸¶¨ÑÕÉ«Öµ}
    procedure Colorize(Color: TCnColor); overload;
    {* µ÷Õûµ±Ç°Í¼Ïñ°´Ö¸¶¨ÑÕÉ«²ÊÉ«»¯£¬Ö§³ÖÍ¼ÏñÍ¸Ã÷ÉèÖÃ
     |<BR>
     |<BR> Color: TCnColor  Ö¸¶¨ÑÕÉ«Öµ}

    // Í¼Ïñ¼¸ºÎ±ä»»
    procedure Flip(Horizontal: Boolean);
    {* ½«µ±Ç°Í¼Ïñ¼¸ºÎÎ»ÖÃ·­×ª
     |<BR>
     |<BR> Horizontal: Boolean  ÎªÕæ±íÊ¾Ë®Æ½·­×ª£¬Îª¼Ù±íÊ¾´¹Ö±·­×ª}
    procedure Turn(Angle: TTurnAngle);
    {* ½«µ±Ç°Í¼Ïñ¼¸ºÎÎ»ÖÃĞı×ª
     |<BR>
     |<BR> Angle: TTurnAngle  ×ª¶¯½Ç¶È¡¢¿ÉÎª ta90¡¢ta180¡¢ta270}

    procedure VShift(Amount: Integer);
    {* ½«µ±Ç°Í¼Ïñ½øĞĞ´¹Ö±Æ½ÒÆ
     |<BR>
     |<BR> Amount: Integer  Æ½ÒÆÁ¿£¬ÔÊĞíÎª¸º}
    procedure HShift(Amount: Integer);
    {* ½«µ±Ç°Í¼Ïñ½øĞĞË®Æ½Æ½ÒÆ
     |<BR>
     |<BR> Amount: Integer  Æ½ÒÆÁ¿£¬ÔÊĞíÎª¸º}
    procedure Rotate(DstCenter: TPoint; Src: TCnBitmap; Angle: Double);
    {* ½«Ô´Í¼ÏñĞı×ªºó»æÖÆµ½µ±Ç°Í¼ÏñÖ¸¶¨Î»ÖÃÉÏ£¬Ö§³ÖÔ´Í¼ÏñÍ¸Ã÷ÊôĞÔ
     |<BR>
     |<BR> DstCenter: TPoint   Ä¿±êÖĞĞÄµãÎ»ÖÃ£¬ÔÊĞíÔÚµ±Ç°Í¼ÏñÍâ
     |<BR> Src: TCnBitmap      Ô´Í¼Ïñ
     |<BR> Angle: Double       Ô´Í¼ÏñĞı×ª½Ç¶È£¬µ¥Î»Îª¶ÈÊı}

    // ÂË¾µ´¦Àí·½·¨
    procedure ApplyFilter(Core: TFilterCore; Cent: Integer = 0);
    {* ¶Ôµ±Ç°Í¼Ïñ°´Ö¸¶¨Ä£°å½øĞĞ¾í»ıÔËËã
     |<BR>
     |<BR> Core: TFilterCore  3x3 ¾í»ıºË
     |<BR> Cent: Integer       ¾í»ı½á¹ûÒòËØ}
    procedure Blur;
    {* ¶Ôµ±Ç°Í¼Ïñ½øĞĞÄ£ºı´¦Àí£¬Ê¹ÓÃ 3x3 ¾ùÖµËã×Ó}
    procedure GaussianBlur(Amount: Integer);
    {* ¶Ôµ±Ç°Í¼Ïñ½øĞĞ¿ìËÙ¸ßË¹Ä£ºı´¦Àí
     |<BR>
     |<BR> Amount: Integer    Ä£ºı°ë¾¶}
    procedure Sharpen;
    {* ¶Ôµ±Ç°Í¼Ïñ½øĞĞÈñ»¯´¦Àí}
    procedure SharpenMore(Amount: Integer);
    {* ¶Ôµ±Ç°Í¼Ïñ½øĞĞ¸ü¶àµÄÈñ»¯´¦Àí
     |<BR>
     |<BR> Amount: Integer    Èñ»¯³Ì¶È}
    procedure Spray(Amount: Integer);
    {* ¶Ôµ±Ç°Í¼Ïñ½øĞĞÅç½¦ÂË¾µ´¦Àí}
    procedure Emboss;
    {* ½«µ±Ç°Í¼Ïñ¸¡µñ»¯}
    procedure Posterize(Amount: Integer);
    {* ²úÉú±Ú»­Ğ§¹û}
    procedure HeightMap(Amount: Integer);
    {* ²úÉúµØÍ¼Ğ§¹û}
    procedure Marble(Scale: Double; Turbulence: Integer);
    {* ²úÉúË®µÎĞ§¹û}
    procedure Wave(XDiv, YDiv, RatioVal: Double; Wrap: Boolean);
    {* ¶Ôµ±Ç°Í¼Ïñ½øĞĞÅ¤Çú±äĞÎ
     |<BR>
     |<BR> XDiv, YDiv: Double    Ë®Æ½¡¢´¹Ö±·½ÏòµÄÅ¤ÇúÏµÊı
     |<BR> RatioVal: Double      Å¤Çú³Ì¶È
     |<BR> Wrap: Boolean         Ö¸¶¨ÊÇ·ñ×Ô¶¯»·ÈÆ}
    procedure Mosaic(xAmount, yAmount: Integer);
    {* ½«µ±Ç°Í¼ÏñÂíÈü¿Ë»¯
     |<BR>
     |<BR> xAmount: Integer    ¾ØĞÎ¿é¿í¶È
     |<BR> yAmount: Integer    ¾ØĞÎ¿é¸ß¶È}
    procedure Twist(Amount: Integer);
    {* ½«µ±Ç°Í¼Ïñ×ª»¯ÎªĞıÎĞÍ¼
     |<BR>
     |<BR> Amount: Integer     °ë¾¶ÏµÊı}
    procedure Lighting(Center: TPoint; OffX, OffY: Integer; Angle: Double;
      Color: TColor; Amount: TCnAlpha); overload;
    {* ÔÚµ±Ç°Í¼ÏñÉÏ²úÉú¹âÕÕĞ§¹û
     |<BR>
     |<BR> Center: TPoint       ¹âÕÕÖĞĞÄµã
     |<BR> OffX, OffY: Integer  ¹âÕÕ·¶Î§£¬³¤¡¢¶ÌÖá°ë¾¶
     |<BR> Angle: Double        ½Ç¶È£¬OffX Ö¸¶¨µÄ³¤ÖáÓëË®Æ½ÖáµÄ¼Ğ½Ç
     |<BR> Color: TColor        ¹âÕÕÑÕÉ«
     |<BR> Amount: TCnAlpha     ¹âÕÕÇ¿¶È}
    procedure Lighting(Rect: TRect; Data: TCnLighting); overload;
    {* ÔÚµ±Ç°Í¼ÏñÉÏ²úÉú¹âÕÕĞ§¹û
     |<BR>
     |<BR> Rect: TRect        ¹âÕÕÄ¿±ê·¶Î§£¨Î´Ğı×ªÇ°£©
     |<BR> Data: TCnLighting  ¹âÕÕ²ÎÊı}
    procedure Mask(MaskColor: TCnColor); overload;
    {* ½«µ±Ç°Í¼Ïñ°´Ö¸¶¨ÑÕÉ«Îª±ê×¼¶şÖµ»¯
     |<BR>
     |<BR> MaskColor: TCnColor    Ö¸¶¨µÄÑÕÉ«£¬Óë¸ÃÑÕÉ«ÏàÍ¬µÄ±äÎª°×É«£¬·´Ö®ÎªºÚÉ«}
    procedure MaskEx(MaskColor, InColor, BackColor: TCnColor); overload;
    {* ½«µ±Ç°Í¼Ïñ°´Ö¸¶¨ÑÕÉ«Îª±ê×¼¶şÖµ»¯
     |<BR>
     |<BR> MaskColor: TCnColor    Ö¸¶¨µÄÑÕÉ«
     |<BR> InColor: TCnColor      Í¼ÏñÖĞÓëÖ¸¶¨É«ÏàÍ¬µÄÏñËØÓÃ¸ÃÑÕÉ«Ìæ´ú
     |<BR> BackColor: TCnColor    Í¼ÏñÖĞÓëÖ¸¶¨É«²»Í¬µÄÏñËØÓÃ¸ÃÑÕÉ«Ìæ´ú}
    procedure Mask(MaskColor: TColor); overload;
    {* ½«µ±Ç°Í¼Ïñ°´Ö¸¶¨ÑÕÉ«Îª±ê×¼¶şÖµ»¯
     |<BR>
     |<BR> MaskColor: TColor    Ö¸¶¨µÄÑÕÉ«£¬Óë¸ÃÑÕÉ«ÏàÍ¬µÄ±äÎª°×É«£¬·´Ö®ÎªºÚÉ«}
    procedure MaskEx(MaskColor, InColor, BackColor: TColor); overload;
    {* ½«µ±Ç°Í¼Ïñ°´Ö¸¶¨ÑÕÉ«Îª±ê×¼¶şÖµ»¯
     |<BR>
     |<BR> MaskColor: TColor    Ö¸¶¨µÄÑÕÉ«
     |<BR> InColor: TColor      Í¼ÏñÖĞÓëÖ¸¶¨É«ÏàÍ¬µÄÏñËØÓÃ¸ÃÑÕÉ«Ìæ´ú
     |<BR> BackColor: TColor    Í¼ÏñÖĞÓëÖ¸¶¨É«²»Í¬µÄÏñËØÓÃ¸ÃÑÕÉ«Ìæ´ú}
    procedure AddColorNoise(Amount: Integer);
    {* ÔÚµ±Ç°Í¼ÏñÖĞÔö¼Ó²ÊÉ«ÔëÉùµã
     |<BR>
     |<BR> Amount: Integer    ÔëÉùÏµÊı}
    procedure AddMonoNoise(Amount: Integer);
    {* ÔÚµ±Ç°Í¼ÏñÖĞÔö¼ÓºÚ°×ÔëÉùµã
     |<BR>
     |<BR> Amount: Integer    ÔëÉùÏµÊı}
    procedure RemoveNoise(Amount: Integer);
    {* ´Óµ±Ç°Í¼ÏñÖĞÒÆÈ¥ÔëÉùµã£¬ÓÃÓÚÍ¼Ïñ½µÔë´¦Àí
     |<BR>
     |<BR> Amount: Integer    ÔëÉùÏµÊı}
    procedure AddMiddleColor(Color: TColor);
    {* ½«µ±Ç°Í¼ÏñÓëÖ¸¶¨ÑÕÉ«×ö¾ùÖµÔËËã£¬ÀàËÆÓÚÃÉ°å´¦Àí
     |<BR>
     |<BR> Color: TColor    Ç°¾°ÑÕÉ«}
    procedure AddMiddleColorEx(Color: TColor; Rect: TRect);
    {* ½«µ±Ç°Í¼ÏñµÄÖ¸¶¨ÇøÓòÓëÖ¸¶¨ÑÕÉ«×ö¾ùÖµÔËËã£¬ÀàËÆÓÚÃÉ°å´¦Àí
     |<BR>
     |<BR> Color: TColor    Ç°¾°ÑÕÉ«
     |<BR> Rect: TRect      Ö¸¶¨¾ØĞÎ}

    // ÆäËüÍ¼Ïñ´¦Àí·½·¨
    procedure InterpolateRect(Rect: TRect; c00, c10, c01, c11: TCnColor); overload;
    {* ¸ù¾İËÄ½ÇÑÕÉ«ÖµÓÃ½¥±äÉ«Ìî³ä¾ØĞÎ
     |<BR>
     |<BR> Rect: TRect      ¾ØĞÎ¿é
     |<BR> c00: TCnColor    ×óÉÏ½ÇÑÕÉ«
     |<BR> c10: TCnColor    ÓÒÉÏ½ÇÑÕÉ«
     |<BR> c01: TCnColor    ×óÏÂ½ÇÑÕÉ«
     |<BR> c11: TCnColor    ÓÒÉÏ½ÇÑÕÉ«}
    procedure InterpolateRect(Rect: TRect; c00, c10, c01, c11: TColor); overload;
    {* ¸ù¾İËÄ½ÇÑÕÉ«ÖµÓÃ½¥±äÉ«Ìî³ä¾ØĞÎ
     |<BR>
     |<BR> Rect: TRect    ¾ØĞÎ¿é
     |<BR> c00: TColor    ×óÉÏ½ÇÑÕÉ«
     |<BR> c10: TColor    ÓÒÉÏ½ÇÑÕÉ«
     |<BR> c01: TColor    ×óÏÂ½ÇÑÕÉ«
     |<BR> c11: TColor    ÓÒÉÏ½ÇÑÕÉ«}

    // ¿¹¾â³İ»­±Ê»æÖÆ·½·¨£¨Ö§³ÖĞ¡Êı£©
    procedure DrawLineF(x1, y1, x2, y2: Single; Color: TColor);
    {* ÒÔÖ¸¶¨ÑÕÉ«»æÖÆÒ»ÌõÖ±Ïß£¬Ê¹ÓÃ¿¹¾â³İËã·¨
     |<BR>
     |<BR> x1, y1: Single    ÆğÊ¼µã×ø±ê
     |<BR> x2, y2: Single    ½áÊøµã×ø±ê
     |<BR> Color: TColor     Ö±ÏßÑÕÉ«}
    procedure LineToF(x, y: Single); overload;
    {* ´Óµ±Ç°µãPenPosF»æÖÆÖ±Ïßµ½Ä¿±êµã£¬Í¬Ê±ÒÆ¶¯»­±Ê×ø±ê£¬Ê¹ÓÃ¿¹¾â³İËã·¨
     |<BR>
     |<BR> x, y: Single      Ä¿±êµã×ø±ê}
    procedure LineToF(Point: TPointF); overload;
    {* ´Óµ±Ç°µãPenPosF»æÖÆÖ±Ïßµ½Ä¿±êµã£¬Í¬Ê±ÒÆ¶¯»­±Ê×ø±ê£¬Ê¹ÓÃ¿¹¾â³İËã·¨
     |<BR>
     |<BR> Point: TPointF    Ä¿±êµã×ø±ê}
    procedure MoveToF(x, y: Single); overload;
    {* ÒÆ¶¯µ±Ç°»­±Êµ½Ä¿±êµã
     |<BR>
     |<BR> x, y: Single      Ä¿±êµã×ø±ê}
    procedure MoveToF(Point: TPointF); overload;
    {* ÒÆ¶¯µ±Ç°»­±Êµ½Ä¿±êµã
     |<BR>
     |<BR> Point: TPointF    Ä¿±êµã×ø±ê}
    procedure DrawRectF(const Rect: TRectF);
    {* Ê¹ÓÃ»­±Ê»æÖÆÒ»¸ö¾ØĞÎ£¬Ê¹ÓÃ¿¹¾â³İËã·¨
     |<BR>
     |<BR> Rect: TRectF    Ä¿±ê¾ØĞÎ}
    procedure PolylineF(const Points: TPointFArray);
    {* Ê¹ÓÃ»­±Ê»æÖÆÕÛÏß£¬Ê¹ÓÃ¿¹¾â³İËã·¨
     |<BR>
     |<BR> Points: TPointFArray    ¸÷¶¥µã×ø±êÊı×é}
    procedure EllipseF(x1, y1, x2, y2: Single); overload;
    {* Ê¹ÓÃ»­±Ê»æÖÆÍÖÔ²£¬Ê¹ÓÃ¿¹¾â³İËã·¨
     |<BR>
     |<BR> x1, y1: Single    Íâ½Ó¾ØĞÎµÄ×óÉÏ½Ç×ø±ê
     |<BR> x2, y2: Single    Íâ½Ó¾ØĞÎµÄÓÒÏÂ½Ç×ø±ê}
    procedure EllipseF(const Rect: TRectF); overload;
    {* Ê¹ÓÃ»­±Ê»æÖÆÍÖÔ²£¬Ê¹ÓÃ¿¹¾â³İËã·¨
     |<BR>
     |<BR> Rect: TRectF    Íâ½Ó¾ØĞÎ}

    // Æ½»¬×ÖÌå»æÖÆ·½·¨
    function TextExtent(const Text: string): TSize;
    {* ¼ÆËãÎÄ±¾ÏÔÊ¾ÇøÓò£¬Ê¹ÓÃÆ½»¬×ÖÌå Font ÊôĞÔ£¬²»Ö§³Ö¶àĞĞÎÄ±¾
     |<BR>
     |<BR> Text: string    ÎÄ±¾ÄÚÈİ
     |<BR> Result: TSize    ÎÄ±¾ÇøÓò}
    function TextHeight(const Text: string): Integer;
    {* ¼ÆËãÎÄ±¾ÏÔÊ¾¸ß¶È£¬Ê¹ÓÃÆ½»¬×ÖÌå Font ÊôĞÔ£¬²»Ö§³Ö¶àĞĞÎÄ±¾
     |<BR>
     |<BR> Text: string      ÎÄ±¾ÄÚÈİ
     |<BR> Result: Integer    ÎÄ±¾ÏÔÊ¾¸ß¶È}
    function TextWidth(const Text: string): Integer;
    {* ¼ÆËãÎÄ±¾ÏÔÊ¾¿í¶È£¬Ê¹ÓÃÆ½»¬×ÖÌå Font ÊôĞÔ£¬²»Ö§³Ö¶àĞĞÎÄ±¾
     |<BR>
     |<BR> Text: string      ÎÄ±¾ÄÚÈİ
     |<BR> Result: Integer    ÎÄ±¾ÏÔÊ¾¿í¶È}
    procedure TextOut(x, y: Integer; const Text: string);
    {* ÔÚµ±Ç°µ±Ç°Í¼ÏñÖĞ»æÖÆÎÄ±¾£¬Ê¹ÓÃÆ½»¬×ÖÌå Font ÊôĞÔ£¬²»Ö§³Ö¶àĞĞÎÄ±¾
     |<BR> FontClear ÊôĞÔ¾ö¶¨ÎÄ±¾±³¾°ÊÇ·ñÍ¸Ã÷£¬FontBkColor Îª²»Í¸Ã÷Ê±µÄ±³¾°Ìî³äÉ«
     |<BR>
     |<BR> x, y: Integer    ÎÄ±¾×óÉÏ½Ç×ø±ê
     |<BR> Text: string      ÎÄ±¾ÄÚÈİ}

    // ¸ß¼¶ÊôĞÔ
    property Handle: HBITMAP read GetHandle;
    {* µ±Ç°Î»Í¼ HBITMAP ¾ä±ú£¬Ö»¶ÁÊôĞÔ¡£Èç¹ûÎ»Í¼Îª¿Õ£¬½«³öÏÖÒì³£}
    property DC: HDC read GetDC;
    {* µ±Ç°Î»Í¼ DC ¾ä±ú£¬Ö»¶ÁÊôĞÔ¡£Èç¹ûÎ»Í¼Îª¿Õ£¬½«³öÏÖÒì³£}
    property Bits: Pointer read FBits;
    {* µ±Ç°Î»Í¼ÏñËØÊı¾İ´æ·ÅµÄµØÖ·£¬Ö»¶ÁÊôĞÔ¡£Èç¹ûÎ»Í¼Îª¿Õ£¬·µ»Ønil}
    property Size: Integer read FSize;
    {* µ±Ç°Î»Í¼ÏñËØÊı¾İ¿éµÄ´óĞ¡£¬Ö»¶ÁÊôĞÔ¡£Èç¹ûÎ»Í¼Îª¿Õ£¬·µ»Ø 0}
    property GdiAllocStyle: TGdiAllocStyle read FGdiAllocStyle write FGdiAllocStyle;
    {* µ±Ç°Î»Í¼µÄGDI×ÊÔ´¹ÜÀí·½Ê½£¬¸ß¼¶ÊôĞÔ}
    property ScanLine[Row: Integer]: PCnLine read GetScanLine;
    {* È¡µÃµ±Ç°Î»Í¼Ò»ĞĞÉ¨ÃèÏßµØÖ·£¬Ö»¶ÁÊôĞÔ¡£Èç¹ûÎ»Í¼Îª¿Õ»ò·¶Î§³¬ÏŞ£¬½«³öÏÖÒì³£}
    property Pixels[x, y: Integer]: TCnColor read GetPixel write SetPixel;
    {* ·ÃÎÊÎ»Í¼ÖĞµÄÄ³¸öÏñËØ¡£Èç¹ûÎ»Í¼Îª¿Õ»ò·¶Î§³¬ÏŞ£¬½«³öÏÖÒì³£}

    // ³£¹æÊôĞÔ
    property Width: Integer read FWidth write SetWidth;
    {* µ±Ç°Î»Í¼µÄ¿í¶È}
    property Height: Integer read FHeight write SetHeight;
    {* µ±Ç°Î»Í¼µÄ¸ß¶È}
    property ClientRect: TRect read GetClientRect;
    {* µ±Ç°Î»Í¼µÄÕû¸öÇøÓò£¬Ö»¶ÁÊôĞÔ}
    property Canvas: TCnCanvas read GetCanvas;
    {* ·ÃÎÊµ±Ç°Î»Í¼µÄ»­²¼£¬Ö»¶ÁÊôĞÔ}
    property Empty: Boolean read GetEmpty;
    {* µ±Ç°Î»Í¼ÊÇ·ñÎª¿Õ£¬Ö»¶ÁÊôĞÔ}
    property Font: TCnFont read GetFont write SetFont;
    {* Æ½»¬×ÖÌåÊôĞÔ£¬ÅÉÉú×ÔTFont£¬Ìá¹©Ò»Ğ©ÌØĞ§ÏÔÊ¾²ÎÊı}
    property FontClear: Boolean read FFontClear write FFontClear default False;
    {* »æÖÆÆ½»¬×ÖÌåÎÄ±¾Ê±£¬±³¾°ÊÇ·ñÍ¸Ã÷}
    property FontBkColor: TColor read FFontBkColor write FFontBkColor default clWhite;
    {* »æÖÆÆ½»¬×ÖÌåÎÄ±¾Ê±£¬Èç¹û±³¾°²»Í¸Ã÷£¬ÓÃÓÚÌî³ä±³¾°µÄÑÕÉ«}
    property SmoothFilter: Boolean read FSmoothFilter write FSmoothFilter default True;
    {* ÔÚ¶ÔÍ¼Ïñ½øĞĞËõ·Å¡¢Ğı×ªµÈ¼¸ºÎ±ä»»Ê±£¬ÊÇ·ñÊ¹ÓÃ¿¹¾â³İËã·¨½øĞĞÆ½»¬´¦Àí}

    // ¿¹¾â³İÍ¼ĞÎ»æÖÆÊôĞÔ
    property PixelsF[x, y: Single]: TCnColor read GetPixelsF write SetPixelsF;
    {* ·ÃÎÊÎ»Í¼ÖĞµÄĞ¡Êı×ø±êµÄÏñËØ¡£Èç¹ûÎ»Í¼Îª¿Õ»ò·¶Î§³¬ÏŞ£¬½«³öÏÖÒì³£}
    property PenPosF: TPointF read FPenPosF write FPenPosF;
    {* ÔÚ¿¹¾â³İÍ¼ĞÎ»æÖÆÖĞ£¬µ±Ç°»­±ÊµÄÎ»ÖÃ}
    property PenColor: TColor read FPenColor write FPenColor default clBlack;
    {* ÔÚ¿¹¾â³İÍ¼ĞÎ»æÖÆÖĞ£¬µ±Ç°»­±ÊµÄÑÕÉ«}
    property PenWeight: TPenWeight read FPenWeight write FPenWeight default pwNormal;
    {* ÔÚ¿¹¾â³İÍ¼ĞÎ»æÖÆÖĞ£¬µ±Ç°»­±ÊµÄ´ÖÏ¸³Ì¶È}
  published
    property Transparent: Boolean read FTransparent write FTransparent default False;
    {* Í¼ÏñµÄÍ¸Ã÷ÊôĞÔ£¬ÔÚËùÓĞµÄÍ¼Ïñ»æÖÆ¹ı³ÌÖĞÓĞĞ§£¬¸ù¾İ TransparentColor À´ÅĞ¶Ï}
    property TransparentColor: TColor read FTransparentColor write
      FTransparentColor default clDefault;
    {* Í¼ÏñµÄÍ¸Ã÷É«ÊôĞÔ£¬Î»Í¼ÖĞÓë¸ÃÑÕÉ«ÏàÍ¬µÄÏñËØµã°´Í¸Ã÷´¦Àí¡£
     |<BR> µ±ÖµÎª clDefault Ê±£¬Ê¹ÓÃÍ¼Ïñ×óÏÂ½ÇÏñËØÑÕÉ«ÖµÀ´´úÌæ¡£}
  end;

  TCnColorSpaceConverter = class
  {* ÑÕÉ«¿Õ¼ä×ª»»Æ÷Àà£¬¸ºÔğ RGB Óë YCbCr ÑÕÉ«¿Õ¼äµÄÏà»¥×ª»»}
  private
    function ClipByte(Value: Double): Byte;
  public
    constructor Create;
    {* ¹¹Ôìº¯Êı}
    destructor Destroy; override;
    {* Îö¹¹º¯Êı}

    procedure RGBToYCbCr(const R, G, B: Byte; out Y, Cb, Cr: Byte);
    {* RGB ×ª YCbCr£¨ITU-R BT.601 ±ê×¼£©¡£

       R ºìÉ«·ÖÁ¿£¨0-255£©
       G ÂÌÉ«·ÖÁ¿£¨0-255£©
       B À¶É«·ÖÁ¿£¨0-255£©
       Y Êä³öÁÁ¶È·ÖÁ¿£¨0-255£©
       Cb Êä³öÀ¶É«É«¶È·ÖÁ¿£¨0-255£©
       Cr Êä³öºìÉ«É«¶È·ÖÁ¿£¨0-255£©
    }

    procedure YCbCrToRGB(const Y, Cb, Cr: Byte; out R, G, B: Byte);
    {* YCbCr ×ª RGB¡£

       Y ÁÁ¶È·ÖÁ¿£¨0-255£©
       Cb À¶É«É«¶È·ÖÁ¿£¨0-255£©
       Cr ºìÉ«É«¶È·ÖÁ¿£¨0-255£©
       R Êä³öºìÉ«·ÖÁ¿£¨0-255£©
       G Êä³öÂÌÉ«·ÖÁ¿£¨0-255£©
       B Êä³öÀ¶É«·ÖÁ¿£¨0-255£©
    }

    procedure ConvertBitmapToYCbCr(Source: TBitmap;
      YPlane, CbPlane, CrPlane: TCnFloatMatrix);
    {* ÅúÁ¿×ª»»Õû¸öÎ»Í¼µ½ YCbCr ÑÕÉ«¿Õ¼ä¡£

       Source Ô´Î»Í¼£¨24 Î» RGB£©
       YPlane Êä³ö Y Í¨µÀ¾ØÕó
       CbPlane Êä³ö Cb Í¨µÀ¾ØÕó
       CrPlane Êä³ö Cr Í¨µÀ¾ØÕó
    }

    procedure ConvertYCbCrToBitmap(YPlane, CbPlane, CrPlane: TCnFloatMatrix;
      Dest: TBitmap);
    {* ÅúÁ¿×ª»» YCbCr Êı¾İµ½Î»Í¼¡£

       YPlane Y Í¨µÀ¾ØÕó
       CbPlane Cb Í¨µÀ¾ØÕó
       CrPlane Cr Í¨µÀ¾ØÕó
       Dest Êä³öÎ»Í¼£¨24 Î» RGB£©
    }

    procedure ExtractYChannel(Source: TBitmap; YPlane: TCnFloatMatrix);
    {* ÌáÈ¡Î»Í¼µÄ Y Í¨µÀ£¨ÁÁ¶ÈÍ¨µÀ£©¡£

       Source Ô´Î»Í¼£¨24 Î» RGB£©
       YPlane Êä³ö Y Í¨µÀ¾ØÕó
    }
  end;

procedure FreeBmpDC;
{* ÊÍ·Å³ÌĞòÖĞËùÓĞÎ»Í¼ÒÑ·ÖÅäµÄ DC ¾ä±ú}
procedure FreeBmpHandle(All: Boolean);
{* ÊÍ·Å³ÌĞòÖĞËùÓĞÎ»Í¼ÒÑ·ÖÅäµÄ HBITMAP ¾ä±ú£¬Èç¹û²ÎÊıÎª¼Ù£¬¸ù¾İ GdiAllocStyle ÊôĞÔÅĞ¶Ï}

//--------------------------------------------------------//
// ¹«ÓÃÔËĞĞÊ±¼ä¹ı³Ì¿â                                     //
//--------------------------------------------------------//

var
  HSLRange: Integer = 240;

// HSL ÑÕÉ«Óë RGB É«×ª»»º¯Êı
function HSLToRGB(H, S, L: Double): TColor;
{* HSL ÑÕÉ«×ª»»Îª RGB ÑÕÉ«
 |<BR>
 |<BR> H, S, L: Double    ·Ö±ğÎªÉ«µ÷¡¢±¥ºÍ¶È¡¢ÁÁ¶È·ÖÁ¿£¬Îª"0"µ½"1"Ö®¼äµÄĞ¡Êı
 |<BR> Result: TColor      ·µ»ØRGBÑÕÉ«Öµ}
function HSLRangeToRGB(H, S, L: Integer): TColor;
{* HSL ÑÕÉ«×ª»»Îª RGB ÑÕÉ«
 |<BR>
 |<BR> H, S, L: Integer    ·Ö±ğÎªÉ«µ÷¡¢±¥ºÍ¶È¡¢ÁÁ¶È·ÖÁ¿£¬0..240
 |<BR> Result: TColor      ·µ»ØRGBÑÕÉ«Öµ}
procedure RGBToHSL(Color: TColor; out H, S, L: Double);
{* RGB ÑÕÉ«×ª»»Îª HSL ÑÕÉ«
 |<BR>
 |<BR> Color: TColor      RGB ÑÕÉ«Öµ
 |<BR> H, S, L: Integer    Êä³ö·Ö±ğÎªÉ«µ÷¡¢±¥ºÍ¶È¡¢ÁÁ¶È·ÖÁ¿£¬Îª"0"µ½"1"Ö®¼äµÄĞ¡Êı}
procedure RGBToHSLRange(Color: TColor; out H, S, L: Integer);
{* RGB ÑÕÉ«×ª»»Îª HSL ÑÕÉ«
 |<BR>
 |<BR> Color: TColor      RGB ÑÕÉ«Öµ
 |<BR> H, S, L: Integer    Êä³ö·Ö±ğÎªÉ«µ÷¡¢±¥ºÍ¶È¡¢ÁÁ¶È·ÖÁ¿£¬0..240}

// CMY ÑÕÉ«Óë RGB É«×ª»»º¯Êı
function CMYToRGB(const C, M, Y: Byte): TColor;
{* CMY ÑÕÉ«×ª»»Îª RGB ÑÕÉ«
 |<BR>
 |<BR> C, M, Y: Byte      ·Ö±ğÎª Cyan Çà¡¢Magenta Æ·ºì¡¢Yellow »Æ·ÖÁ¿£¬0..255
 |<BR> Result: TColor      ·µ»Ø RGB ÑÕÉ«Öµ}
procedure RGBToCMY(const RGB: TColor; out C, M, Y: Byte);
{* RGB ÑÕÉ«×ª»»Îª CMY ÑÕÉ«
 |<BR>
 |<BR> Color: TColor      RGB ÑÕÉ«Öµ
 |<BR> C, M, Y: Byte      Êä³ö·Ö±ğÎª Cyan Çà¡¢Magenta Æ·ºì¡¢Yellow »Æ·ÖÁ¿£¬0..255}

// CMYK ÑÕÉ«Óë RGB É«×ª»»º¯Êı
function CMYKToRGB(const C, M, Y, K: Byte): TColor;
{* CMYK ÑÕÉ«×ª»»Îª RGB ÑÕÉ«
 |<BR>
 |<BR> C, M, Y, K: Byte    ·Ö±ğÎª Cyan Çà¡¢Magenta Æ·ºì¡¢Yellow »Æ¡¢Black ºÚ·ÖÁ¿£¬0..255
 |<BR> Result: TColor      ·µ»Ø RGB ÑÕÉ«Öµ}
procedure RGBToCMYK(const RGB: TColor; out C, M, Y, K: Byte);
{* RGB ÑÕÉ«×ª»»Îª CMY ÑÕÉ«
 |<BR>
 |<BR> Color: TColor      RGB ÑÕÉ«Öµ
 |<BR> C, M, Y, K: Byte    Êä³ö·Ö±ğÎª Cyan Çà¡¢Magenta Æ·ºì¡¢Yellow »Æ¡¢Black ºÚ·ÖÁ¿£¬0..255}

// ÔöÇ¿µÄÑÕÉ«´¦Àíº¯Êı
function Gray(Intensity: Byte): TColor;
{* ·µ»ØÒ»¸ö»Ò¶È RGB ÑÕÉ«Öµ}
function Intensity(Color: TColor): Byte;
{* ¼ÆËã RGB ÑÕÉ«ÖµµÄ»Ò¶ÈÖµ}
function RandomColor: TColor;
{* ·µ»ØÒ»¸öËæ»ú RGB ÑÕÉ«Öµ}
procedure DeRGB(Color: TColor; var r, g, b: Byte);
{* ½« Color ·Ö½âÎª r¡¢g¡¢b ÑÕÉ«·ÖÁ¿}

// CnColorÑÕÉ«´¦Àíº¯Êı
function CnColor(r, g, b: Byte): TCnColor; overload;
{* ¸ù¾İ r¡¢g¡¢b ÑÕÉ«·ÖÁ¿·µ»ØÒ»¸ö TCnColor ÑÕÉ«Öµ}
function CnColor(Color: TColor): TCnColor; overload;
{* ×ª»» TColor ÑÕÉ«ÎªÒ»¸ö TCnColor ÑÕÉ«Öµ£¬ÔÊĞíÊ¹ÓÃÏµÍ³ÑÕÉ«Öµ}
function CnGray(Intensity: Byte): TCnColor;
{* ·µ»ØÒ»¸ö»Ò¶È¼¶µÄ TCnColor ÑÕÉ«Öµ}
function CnWinColor(RGB: TCnColor): TColor;
{* ×ª»» TCnColor ÑÕÉ«ÎªÒ»¸ö TColor ÑÕÉ«Öµ}
function CnColorEqu(RGB1, RGB2: TCnColor): Boolean;
{* ÅĞ¶ÏÁ½¸ö TCnColor ÑÕÉ«ÊÇ·ñÏàµÈ}

function PointF(x, y: Single): TPointF;
{* ·µ»ØÒ»¸ö¸¡µãÊı×ø±ê TPointF}
function RectF(Left, Top, Right, Bottom: Single): TRectF;
{* ·µ»ØÒ»¸ö¸¡µãÊı¾ØĞÎ TRectF}

// ´Ó¸¸¿Ø¼ş¸´ÖÆ±³¾°¡£Õâ¸ö¹ı³ÌÀ´×Ô RxLibrary VCLUtils¡£×¢Òâ²¢²»×ÜÊÇÓĞĞ§£¬ÓÈÆäÊÇ IDE ±à¼­Æ÷ÖĞ
procedure CopyControlParentImageToCanvas(AControl: TControl; Dest: TCanvas);

// ³£ÓÃ¾í»ıºË
const
  BlurFilter: TFilterCore = (
    (-1, -1, -1),
    (-1, 1, -1),
    (-1, -1, -1));
  SharpFilter: TFilterCore = (
    (-5, -5, -5),
    (-5, 160, -5),
    (-5, -5, -5));
  EdgeFilter: TFilterCore = (
    (-1, -1, -1),
    (-1, 8, -1),
    (-1, -1, -1));
  EmbossFilter: TFilterCore = (
    (100, 0, 0),
    (0, 0, 0),
    (0, 0, -100));
  Enhance3DFilter: TFilterCore = (
    (-100, 5, 5),
    (5, 5, 5),
    (5, 5, 100));

implementation

type
  TGraphicAccess = class(TGraphic);
  TCnPersistentAccess = class(TCnPersistent);

var
  BitmapList: TThreadList;    // TCnBitmap Î»Í¼ÁĞ±í
  CnCanvasList: TThreadList;  // TCnCanvas ÁĞ±í
  GdiActTimer: TTimer;        // GDI ×ÊÔ´ÊÍ·Å¶¨Ê±Æ÷
  DefGdiAllocStyle: TGdiAllocStyle = gsNormal; // Ä¬ÈÏ GDI ÊÍ·Å·½Ê½
  FreeGdiWaitTime: Cardinal = 3000; // ×Ô¶¯ÊÍ·Å GDI ×ÊÔ´µÈ´ıÊ±¼ä

const
  FreeGdiInterval: Cardinal = 1000; // ×Ô¶¯ÊÍ·ÅGDI×ÊÔ´¶¨Ê±¼ä¸ô
  csItalicAdjust = 0.3;       // Ğ±Ìå×Ö¿í¶ÈĞ£ÕıÏµÊı

type
  TLogPal = record
    lpal: TLogPalette;
    dummy: array[0..255] of TPaletteEntry;
  end;

var
  GrayLogPal: TLogPal;

//--------------------------------------------------------//
// ¹«ÓÃÔËĞĞÊ±¼ä¹ı³Ì¿â                                     //
//--------------------------------------------------------//

// HSL¡¢RGB ×ª»»º¯ÊıËã·¨À´Ô´£º
// http:/www.r2m.com/win-developer-faq/graphics/8.html
// Grahame Marsh 12 October 1997

// HSL ÑÕÉ«×ª»»Îª RGB É«
function HSLToRGB(H, S, L: Double): TColor;
var
  M1, M2: Double;

  function HueToColourValue(Hue: Double): Byte;
  var
    V: Double;
  begin
    if Hue < 0 then
      Hue := Hue + 1
    else if Hue > 1 then
      Hue := Hue - 1;
    if 6 * Hue < 1 then
      V := M1 + (M2 - M1) * Hue * 6
    else if 2 * Hue < 1 then
      V := M2
    else if 3 * Hue < 2 then
      V := M1 + (M2 - M1) * (2 / 3 - Hue) * 6
    else
      V := M1;
    Result := Round(255 * V)
  end;
var
  r, g, b: Byte;
begin
  if S = 0 then
  begin
    r := Round(255 * L);
    g := r;
    b := r
  end else
  begin
    if L <= 0.5 then
      M2 := L * (1 + S)
    else
      M2 := L + S - L * S;
    M1 := 2 * L - M2;
    r := HueToColourValue(H + 1 / 3);
    g := HueToColourValue(H);
    b := HueToColourValue(H - 1 / 3)
  end;
  Result := RGB(r, g, b);
end;

// HSL ÑÕÉ«·¶Î§×ª»»Îª RGB É«
function HSLRangeToRGB(H, S, L: Integer): TColor;
begin
  Result := HSLToRGB(H / (HSLRange - 1), S / HSLRange, L / HSLRange)
end;

// RGB ÑÕÉ«×ªÎª HSL É«
procedure RGBToHSL(Color: TColor; out H, S, L: Double);
var
  r, g, b, D, Cmax, Cmin: Double;
begin
  Color := ColorToRGB(Color);
  r := GetRValue(Color) / 255;
  g := GetGValue(Color) / 255;
  b := GetBValue(Color) / 255;
  Cmax := Max(r, Max(g, b));
  Cmin := Min(r, Min(g, b));
  L := (Cmax + Cmin) / 2;
  if Cmax = Cmin then
  begin
    H := 0;
    S := 0
  end else
  begin
    D := Cmax - Cmin;
    if L < 0.5 then
      S := D / (Cmax + Cmin)
    else
      S := D / (2 - Cmax - Cmin);
    if r = Cmax then
      H := (g - b) / D
    else if g = Cmax then
      H := 2 + (b - r) / D
    else
      H := 4 + (r - g) / D;
    H := H / 6;
    if H < 0 then
      H := H + 1
  end
end;

// RGB ÑÕÉ«×ªÎª HSL É«·¶Î§
procedure RGBToHSLRange(Color: TColor; out H, S, L: Integer);
var
  Hd, Sd, Ld: Double;
begin
  RGBToHSL(Color, Hd, Sd, Ld);
  H := Round(Hd * (HSLRange - 1));
  S := Round(Sd * HSLRange);
  L := Round(Ld * HSLRange);
end;

// CMY ÑÕÉ«Óë RGB É«×ª»»º¯Êı
// Ëã·¨Ìá¹©£ºCnPack ¿ª·¢×é ÌúÄĞ

// CMY ÑÕÉ«×ª»»Îª RGB
function CMYToRGB(const C, M, Y: Byte): TColor;
var
  r, g, b: Byte;
begin
  r := 255 - C;
  g := 255 - M;
  b := 255 - Y;
  Result := RGB(r, g, b);
end;

// RGB ÑÕÉ«×ª»»Îª CMY
procedure RGBToCMY(const RGB: TColor; out C, M, Y: Byte);
var
  r, g, b: Byte;
begin
  DeRGB(RGB, r, g, b);
  C := 255 - r;
  M := 255 - g;
  Y := 255 - b;
end;

// CMYK ÑÕÉ«Óë RGB É«×ª»»º¯Êı
// Ëã·¨Ìá¹©£ºCnPack ¿ª·¢×é ÌúÄĞ

// CMYK ÑÕÉ«×ª»»Îª RGB
function CMYKtoRGB(const C, M, Y, K: Byte): TColor;
var
  r, g, b: Byte;
begin
  r := 255 - (C + K);
  g := 255 - (M + K);
  b := 255 - (Y + K);
  Result := RGB(r, g, b);
end;

// RGB ÑÕÉ«×ª»»Îª CMYK
procedure RGBToCMYK(const RGB: TColor; out C, M, Y, K: Byte);
begin
  RGBToCMY(RGB, C, M, Y);
  K := MinIntValue([C, M, Y]);
  C := C - K;
  M := M - K;
  Y := Y - K;
end;

// ²úÉú»Ò¶ÈÑÕÉ«
function Gray(Intensity: Byte): TColor;
begin
  Result := Intensity shl 16 + Intensity shl 8 + Intensity;
end;

// ¼ÆËãÑÕÉ«ÁÁ¶ÈÖµ
// Ëã·¨À´Ô´£ºGraphic32
// Ëã·¨ĞŞ¸Ä£ºÖÜ¾¢Óğ
function Intensity(Color: TColor): Byte;
asm
// ÊäÈë:  RGB --> EAX
// Êä³ö:  (R * 61 + G * 174 + B * 20) / 256 --> AL
        MOV     ECX,EAX
        AND     EAX,$00FF00FF      // EAX <-   0 B 0 R
        IMUL    EAX,$0014003D
        AND     ECX,$0000FF00      // ECX <-   0 0 G 0
        IMUL    ECX,$0000AE00
        MOV     EDX,EAX
        SHR     ECX,8
        SHR     EDX,16
        ADD     EAX,ECX
        ADD     EAX,EDX
        SHR     EAX,8
end;

// ²úÉúËæ»úÑÕÉ«
function RandomColor: TColor;
begin
  Result := HSLToRGB(Random, 0.75 + Random * 0.25, 0.3 + Random * 0.25);
end;

// È¡ÑÕÉ«RGB·ÖÁ¿
procedure DeRGB(Color: TColor; var r, g, b: Byte);
begin
  Color := ColorToRGB(Color);
  r := GetRValue(Color);
  g := GetGValue(Color);
  b := GetBValue(Color);
end;

// CnColor ÑÕÉ«´¦Àíº¯Êı
function CnColor(r, g, b: Byte): TCnColor;
begin
  Result.r := r;
  Result.g := g;
  Result.b := b;
end;

// ÏµÍ³ÑÕÉ«×ªÎª TCnColor
function CnColor(Color: TColor): TCnColor;
begin
  Color := ColorToRGB(Color);
  Result.r := Color;
  Result.g := Color shr 8;
  Result.b := Color shr 16;
end;

// ²úÉú»Ò¶È¼¶ TCnColor
function CnGray(Intensity: Byte): TCnColor;
begin
  Result.r := Intensity;
  Result.g := Intensity;
  Result.b := Intensity;
end;

// TCnColor ×ªÎª TColor
function CnWinColor(RGB: TCnColor): TColor;
begin
  Result := RGB.b shl 16 + RGB.g shl 8 + RGB.r;
end;

// ÑÕÉ«ÖµÏàµÈ
function CnColorEqu(RGB1, RGB2: TCnColor): Boolean;
begin
  Result := (RGB1.r = RGB2.r) and (RGB1.g = RGB2.g) and (RGB1.b = RGB2.b);
end;

// È¡µã
function PointF(x, y: Single): TPointF;
begin
  Result.x := x;
  Result.y := y;
end;

// È¡¾ØĞÎ
function RectF(Left, Top, Right, Bottom: Single): TRectF;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Right;
  Result.Bottom := Bottom;
end;

//--------------------------------------------------------//
// Ë½ÓĞ¹ı³Ì¿â                                             //
//--------------------------------------------------------//

// ·¶Î§×ª»»
function RangeTran(Range, Low, High, Min, Max: Integer): Integer;
begin
  if (Low = High) or (Min = Max) then
    Result := 0
  else
  begin
    Range := TrimInt(Range, Low, High);
    Result := Round(Min + (Max - Min) / (High - Low) * (Range - Low));
  end;
end;

// ·¶Î§×ª»»ÎªÊµ¼ÊÖµ
function RangeToInt(Range: TAdjustRange; Min, Max: Integer): Integer;
begin
  Result := RangeTran(Range, Low(Range), High(Range), Min, Max);
end;

// Í¸Ã÷¶È×ª»»
function AlphaToInt(Alpha: TCnAlpha): Integer;
begin
  Result := RangeTran(Alpha, Low(Alpha), High(Alpha), 0, 255);
end;

// È¡¾ØĞÎĞı×ªºóÖ®Íâ½Ó¾ØĞÎÓëÄ¿±ê¾ØĞÎÖ®½»
function GetRotateRect(DstRect: TRect; DstCenter: TPoint; W, H: Integer;
  Angle: Double; var Rect: TRect): Boolean;
var
  p1, p2, p3, p4: TPoint;
  FAngle: Double;
  cAngle, sAngle: Double;
  wCos, hCos, wSin, hSin: Double;
  SrcW2, SrcH2: Double;
begin
  FAngle := Angle * Pi / 180;
  sAngle := Sin(FAngle);
  cAngle := Cos(FAngle);

  // ¼ÆËãÄ¿±ê¶¥µãÎ»ÖÃ
  SrcW2 := W / 2 + 1;
  SrcH2 := H / 2 + 1;
  wCos := SrcW2 * cAngle;
  hCos := SrcH2 * cAngle;
  wSin := SrcW2 * sAngle;
  hSin := SrcH2 * sAngle;
  p1.x := Round(-wCos - hSin + DstCenter.x); // ×óÉÏ
  p1.y := Round(-wSin + hCos + DstCenter.y);
  p2.x := Round(wCos - hSin + DstCenter.x); // ÓÒÉÏ
  p2.y := Round(wSin + hCos + DstCenter.y);
  p3.x := Round(-wCos + hSin + DstCenter.x); // ×óÏÂ
  p3.y := Round(-wSin - hCos + DstCenter.y);
  p4.x := Round(wCos + hSin + DstCenter.x); // ÓÒÏÂ
  p4.y := Round(wSin - hCos + DstCenter.y);

  // ¼ÆËã°üº¬¾ØĞÎ
  Rect.Left := MinIntValue([p1.x, p2.x, p3.x, p4.x]) - 1;
  Rect.Right := MaxIntValue([p1.x, p2.x, p3.x, p4.x]) + 1;
  Rect.Top := MinIntValue([p1.y, p2.y, p3.y, p4.y]) - 1;
  Rect.Bottom := MaxIntValue([p1.y, p2.y, p3.y, p4.y]) + 1;
  Result := IntersectRect(Rect, Rect, DstRect);
end;

//--------------------------------------------------------//
// ½¥±äÑÕÉ«Àà                                             //
//--------------------------------------------------------//

{ TCnMiddleColorItem }

// ¸³Öµ
procedure TCnMiddleColorItem.Assign(Source: TPersistent);
begin
  if Source is TCnMiddleColorItem then
  begin
    FColor := TCnMiddleColorItem(Source).FColor;
    FPos := TCnMiddleColorItem(Source).FPos;
    Changed(False);
  end
  else
    inherited;                // TCollectionItem Î´ÊµÏÖ¸Ã·½·¨
end;

// ³õÊ¼»¯
constructor TCnMiddleColorItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FColor := clBlack;
  FPos := 50;
end;

// ÉèÑÕÉ«Öµ
procedure TCnMiddleColorItem.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed(False);
  end;
end;

// ÉèÎ»ÖÃÖµ
procedure TCnMiddleColorItem.SetPos(const Value: TCnGradPos);
begin
  if FPos <> Value then
  begin
    FPos := Value;
    Changed(False);
  end;
end;

{ TCnMiddleColor }

// ³õÊ¼»¯
constructor TCnMiddleColor.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TCnMiddleColorItem);
  FSorting := False;
end;

// Ôö¼ÓÒ»Ïî
procedure TCnMiddleColor.Add(AColor: TColor; APos: TCnGradPos);
begin
  BeginUpdate;
  try
    with TCnMiddleColorItem(inherited Add) do
    begin
      FColor := AColor;
      FPos := APos;
    end;
  finally
    EndUpdate;
  end;
end;

// °´Î»ÖÃÅÅĞò
procedure TCnMiddleColor.Sort;
var
  I, J, Idx, Pos: Integer;
  Item: TCnMiddleColorItem;
begin
  if FSorting then Exit;
  FSorting := True;
  BeginUpdate;
  try
    for I := 0 to Count - 1 do
    begin
      Pos := Items[I].FPos;
      Idx := I;
      for J := I + 1 to Count - 1 do
      begin
        Item := Items[J];
        if Item.FPos < Pos then
        begin
          Pos := Item.FPos;
          Idx := J;
        end;
      end;
      if Idx <> I then
        Items[Idx].Index := I;
    end;
    if GetOwner is TCnPersistent then // Í¨Öª¸üĞÂ
      TCnPersistentAccess(GetOwner).Changed;
  finally
    EndUpdate;
    FSorting := False;
  end;
end;

// ÄÚÈİÒÑ¸üĞÂ
procedure TCnMiddleColor.Update(Item: TCollectionItem);
begin
  inherited;
  Sort;
end;

// ÏÂÃæµÄ·½·¨ÓÃÔÚÉè¼ÆÆÚÊôĞÔ±à¼­Æ÷ÖĞ
// ÏÔÊ¾ÔÚÊôĞÔ±à¼­Æ÷ÖĞµÄÀ¸Ãû
function TCnMiddleColor.GetAttr(Index: Integer): string;
begin
  case Index of
    0: Result := 'Color';
    1: Result := 'Position';
  else
    Result := inherited GetAttr(Index);
  end;
end;

// ÏÔÊ¾ÔÚÊôĞÔ±à¼­Æ÷ÖĞµÄÀ¸Êı
function TCnMiddleColor.GetAttrCount: Integer;
begin
  Result := 2;
end;

// ÏÔÊ¾ÔÚÊôĞÔ±à¼­Æ÷ÖĞµÄ×ÓÏîÄÚÈİ
function TCnMiddleColor.GetItemAttr(Index, ItemIndex: Integer): string;
begin
  case Index of
    0: Result := ColorToString(Items[ItemIndex].FColor);
    1: Result := Format('[%d%%]', [Items[ItemIndex].FPos]);
  else Result := inherited GetItemAttr(Index, ItemIndex);
  end;
end;

// È¡×ÓÏî
function TCnMiddleColor.GetItem(Index: Integer): TCnMiddleColorItem;
begin
  Result := TCnMiddleColorItem(inherited GetItem(Index));
end;

// Éè×ÓÏî
procedure TCnMiddleColor.SetItem(Index: Integer;
  const Value: TCnMiddleColorItem);
begin
  inherited SetItem(Index, Value);
end;

{ TCnGradientColor }

// ¸³Öµ
procedure TCnGradientColor.Assign(Source: TPersistent);
begin
  BeginUpdate;
  try
    if Source is TCnGradientColor then
    begin
      FColorStart := TCnGradientColor(Source).FColorStart;
      FColorEnd := TCnGradientColor(Source).FColorEnd;
      FStyle := TCnGradientColor(Source).FStyle;
      ColorMiddle := TCnGradientColor(Source).FColorMiddle;
    end
    else
      inherited;
  finally
    EndUpdate;
  end;
end;

// ³õÊ¼»¯
constructor TCnGradientColor.Create;
begin
  inherited;
  FColorStart := clBlack;
  FColorEnd := clBlack;
  FStyle := gsLeftToRight;
  FColorMiddle := nil;
end;

// ÊÍ·Å
destructor TCnGradientColor.Destroy;
begin
  if FColorMiddle <> nil then
    FreeAndNil(FColorMiddle);
  inherited;
end;

// È¡ÖĞ¼äÉ«
function TCnGradientColor.GetColorMiddle: TCnMiddleColor;
begin
  if FColorMiddle = nil then
    FColorMiddle := TCnMiddleColor.Create(Self);
  Result := FColorMiddle;
end;

// Éè½áÊøÉ«
procedure TCnGradientColor.SetColorEnd(const Value: TColor);
begin
  if FColorEnd <> Value then
  begin
    FColorEnd := Value;
    Changed;
  end;
end;

// ÉèÖĞ¼äÉ«
procedure TCnGradientColor.SetColorMiddle(const Value: TCnMiddleColor);
begin
  BeginUpdate;
  try
    if (Value <> nil) and (Value.Count > 0) then
      ColorMiddle.Assign(Value) // ×Ô¶¯µ÷ÓÃGet·½·¨±£Ö¤ÊµÀı»¯
    else if FColorMiddle <> nil then // ÎŞÖĞ¼äÉ«Ê±ÊÍ·ÅÊµÀı
      FreeAndNil(FColorMiddle);
  finally
    EndUpdate;
  end;
end;

// ÉèÆğÊ¼É«
procedure TCnGradientColor.SetColorStart(const Value: TColor);
begin
  if FColorStart <> Value then
  begin
    FColorStart := Value;
    Changed;
  end;
end;

// Éè½¥±ä·ç¸ñ
procedure TCnGradientColor.SetStyle(const Value: TCnGradStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Changed;
  end;
end;

//--------------------------------------------------------//
// Æ½»¬ÌØĞ§×ÖÌåÀà                                         //
//--------------------------------------------------------//

{ TCnShadow }

// ¸³Öµ
procedure TCnShadow.Assign(Source: TPersistent);
begin
  BeginUpdate;
  try
    if Source is TCnShadow then
    begin
      FBlur := TCnShadow(Source).FBlur;
      FAlpha := TCnShadow(Source).FAlpha;
      FColor := TCnShadow(Source).FColor;
      FOffsetX := TCnShadow(Source).FOffsetX;
      FOffsetY := TCnShadow(Source).FOffsetY;
    end
    else
      inherited;
  finally
    EndUpdate;
  end;
end;

// ³õÊ¼»¯
constructor TCnShadow.Create;
begin
  inherited;
  FBlur := 1;
  FAlpha := 180;
  FColor := $00444444;
  FOffsetX := 2;
  FOffsetY := 2;
end;

// ÉèÖÃ²»Í¸Ã÷¶È
procedure TCnShadow.SetAlpha(const Value: TCnAlpha);
begin
  if FAlpha <> Value then
  begin
    FAlpha := Value;
    Changed;
  end;
end;

// ÉèÖÃÒõÓ°Ä£ºı
procedure TCnShadow.SetBlur(const Value: TShadowBlur);
begin
  if FBlur <> Value then
  begin
    FBlur := Value;
    Changed;
  end;
end;

// ÉèÖÃÒõÓ°É«
procedure TCnShadow.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

// ÉèÖÃÒõÓ°Ë®Æ½Æ«ÒÆ
procedure TCnShadow.SetOffsetX(const Value: TShadowOffset);
begin
  if FOffsetX <> Value then
  begin
    FOffsetX := Value;
    Changed;
  end;
end;

// ÉèÖÃÒõÓ°´¹Ö±Æ«ÒÆ
procedure TCnShadow.SetOffsetY(const Value: TShadowOffset);
begin
  if FOffsetY <> Value then
  begin
    FOffsetY := Value;
    Changed;
  end;
end;

{ TCnLighting }

// ¸³Öµ
procedure TCnLighting.Assign(Source: TPersistent);
begin
  BeginUpdate;
  try
    if Source is TCnLighting then
    begin
      FAlpha := TCnLighting(Source).FAlpha;
      FColor := TCnLighting(Source).FColor;
      FOffsetX := TCnLighting(Source).FOffsetX;
      FOffsetY := TCnLighting(Source).FOffsetY;
      FWidth := TCnLighting(Source).FWidth;
      FHeight := TCnLighting(Source).FHeight;
      FAngle := TCnLighting(Source).FAngle;
    end
    else
      inherited;
  finally
    EndUpdate;
  end;
end;

// ³õÊ¼»¯
constructor TCnLighting.Create;
begin
  inherited;
  FAlpha := 180;
  FColor := clWhite;
  FOffsetX := 0;
  FOffsetY := 0;
  FWidth := 80;
  FHeight := 80;
  FAngle := 0;
end;

// ÉèÖÃ²»Í¸Ã÷¶È
procedure TCnLighting.SetAlpha(const Value: TCnAlpha);
begin
  if FAlpha <> Value then
  begin
    FAlpha := Value;
    Changed;
  end;
end;

// ÉèÖÃ½Ç¶È
procedure TCnLighting.SetAngle(const Value: Double);
begin
  if FAngle <> Value then
  begin
    FAngle := Value;
    Changed;
  end;
end;

// ÉèÖÃµÆ¹âÑÕÉ«
procedure TCnLighting.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

// ÉèÖÃ¹âÕÕ·¶Î§¿í¶È
procedure TCnLighting.SetWidth(const Value: TLightingRange);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    Changed;
  end;
end;

// ÉèÖÃ¹âÕÕ·¶Î§¸ß¶È
procedure TCnLighting.SetHeight(const Value: TLightingRange);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    Changed;
  end;
end;

// ÉèÖÃ¹âÕÕÖĞĞÄÆ«ÒÆ
procedure TCnLighting.SetOffsetX(const Value: TLightingOffset);
begin
  if FOffsetX <> Value then
  begin
    FOffsetX := Value;
    Changed;
  end;
end;

// ÉèÖÃ¹âÕÕÖĞĞÄÆ«ÒÆ
procedure TCnLighting.SetOffsetY(const Value: TLightingOffset);
begin
  if FOffsetY <> Value then
  begin
    FOffsetY := Value;
    Changed;
  end;
end;

{ TCnFont }

// ¸³Öµ
procedure TCnFont.Assign(Source: TPersistent);
begin
  BeginUpdate;
  try
    if Source is TCnFont then
    begin
      FStyleEx := TCnFont(Source).FStyleEx;
      FNoise := TCnFont(Source).FNoise;
      FSpray := TCnFont(Source).FSpray;
      FAlpha := TCnFont(Source).FAlpha;
      FTextureMode := TCnFont(Source).FTextureMode;
      Quality := TCnFont(Source).FQuality;
      FTexture.Assign(TCnFont(Source).FTexture);
      FShadow.Assign(TCnFont(Source).FShadow);
      FGradient.Assign(TCnFont(Source).FGradient);
    end;
    inherited;
  finally
    EndUpdate;
  end;
end;

// ³õÊ¼»¯
constructor TCnFont.Create;
begin
  inherited;
  FNoise := 0;
  FSpray := 0;
  FAlpha := csMaxAlpha;
  FStyleEx := [];
  FTextureMode := dmTiled;
  FQuality := fqNormal;
  FScale := 3;
  FShadow := TCnShadow.Create(ChildChanged);
  FGradient := TCnGradientColor.Create(ChildChanged);
  FLighting := TCnLighting.Create(ChildChanged);
end;

// ÊÍ·Å
destructor TCnFont.Destroy;
begin
  FLighting.Free;
  FGradient.Free;
  FShadow.Free;
  if FTexture <> nil then FTexture.Free;
  inherited;
end;

// ¿ªÊ¼¸üĞÂ
procedure TCnFont.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

// È¡ËùÓĞÕß
function TCnFont.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

// ½áÊø¸üĞÂ
procedure TCnFont.EndUpdate;
begin
  Assert(FUpdateCount > 0, 'Unpaired TCnFont.EndUpdate');
  Dec(FUpdateCount);
  if (FUpdateCount = 0) then Changed;
end;

// ÊôĞÔÒÑ±ä¸ü
procedure TCnFont.Changed;
begin
  if FUpdateCount = 0 then inherited;
end;

// ×ÓÔªËØ¸üĞÂÍ¨Öª
procedure TCnFont.ChildChanged(Sender: TObject);
begin
  if (Sender is TCnShadow) and (fsShadow in FStyleEx) then
    Changed
  else if (Sender is TCnGradientColor) and (fsGradient in FStyleEx) then
    Changed
  else if (Sender is TCnLighting) and (fsLighting in FStyleEx) then
    Changed
  else if (Sender is TPicture) and (fsTexture in FStyleEx) then
    Changed;
end;

// È¡ÎÆÀí
function TCnFont.GetTexture: TPicture;
begin
  if FTexture = nil then
  begin
    FTexture := TPicture.Create;
    FTexture.OnChange := ChildChanged;
  end;
  Result := FTexture;
end;

// ÉèÖÃÍ¸Ã÷¶È
procedure TCnFont.SetAlpha(const Value: TCnAlpha);
begin
  if FAlpha <> Value then
  begin
    FAlpha := Value;
    Changed;
  end;
end;

// ÉèÖÃ½¥±äÉ«
procedure TCnFont.SetGradient(const Value: TCnGradientColor);
begin
  BeginUpdate;
  try
    FGradient.Assign(Value);
  finally
    EndUpdate;
  end;
end;

// ÉèÖÃÔëÉù
procedure TCnFont.SetNoise(const Value: Byte);
begin
  if FNoise <> Value then
  begin
    FNoise := Value;
    Changed;
  end;
end;

// ÉèÖÃÅç½¦
procedure TCnFont.SetSpray(const Value: Byte);
begin
  if FSpray <> Value then
  begin
    FSpray := Value;
    Changed;
  end;
end;

// ÉèÖÃÒõÓ°
procedure TCnFont.SetShadow(const Value: TCnShadow);
begin
  BeginUpdate;
  try
    FShadow.Assign(Value);
  finally
    EndUpdate;
  end;
end;

// ÉèÖÃ¹âÕÕ
procedure TCnFont.SetLighting(const Value: TCnLighting);
begin
  BeginUpdate;
  try
    FLighting.Assign(Value);
  finally
    EndUpdate;
  end;
end;

// ÉèÖÃÀ©Õ¹·ç¸ñ
procedure TCnFont.SetStyleEx(const Value: TFontStyleExs);
begin
  if FStyleEx <> Value then
  begin
    FStyleEx := Value;
    Changed;
  end;
end;

// ÉèÖÃÎÆÀí
procedure TCnFont.SetTexture(const Value: TPicture);
begin
  BeginUpdate;
  try
    if (Value <> nil) and (Value.Graphic <> nil) and not Value.Graphic.Empty then
      Texture.Assign(Value)
    else if FTexture <> nil then
      FreeAndNil(FTexture)
  finally
    EndUpdate;
  end;
end;

// ÉèÖÃÎÆÀíÄ£Ê½
procedure TCnFont.SetTextureMode(const Value: TCnDrawMode);
begin
  if FTextureMode <> Value then
  begin
    FTextureMode := Value;
    Changed;
  end;
end;

// ÉèÖÃÏÔÊ¾¾«¶È
procedure TCnFont.SetQuality(const Value: TFontQuality);
begin
  if FQuality <> Value then
  begin
    FQuality := Value;
    case FQuality of
      fqHigh: FScale := 4;
      fqNormal: FScale := 3;
      fqLow: FScale := 2;
      fqNone: FScale := 1;
    end;
    Changed;
  end;
end;

{ TCnFontMask }

// Ä£ºı´¦Àí
procedure TCnFontMask.SplitBlur(Amount: Integer);
var
  p1, p2, Dst: PByteArray;
  x1, x2: Integer;
  x, y: Integer;
  Tmp: TCnFontMask;
begin
  Tmp := TCnFontMask.Create;
  try
    CopyTo(Tmp);
    for y := 0 to FHeight - 1 do
    begin
      p1 := Tmp.ScanLine[TrimInt(y + Amount, 0, FHeight - 1)];
      p2 := Tmp.ScanLine[TrimInt(y - Amount, 0, FHeight - 1)];
      Dst := ScanLine[y];
      for x := 0 to FWidth - 1 do
      begin
        x1 := TrimInt(x - Amount, 0, FWidth - 1);
        x2 := TrimInt(x + Amount, 0, FWidth - 1);
        Dst[x] := (p1[x1] + p1[x2] + p2[x1] + p2[x2]) shr 2;
      end;
    end;
  finally
    Tmp.Free;
  end;
end;

// ¸ßË¹Ä£ºı´¦Àí
procedure TCnFontMask.Blur(Amount: Integer);
var
  I: Integer;
begin
  if Amount > 0 then
    for I := Amount downto 1 do
      SplitBlur(I);
end;

// Åç½¦Ğ§¹û
procedure TCnFontMask.Spray(Amount: Integer);
var
  r, x, y: Integer;
begin
  for y := 0 to FHeight - 1 do
    for x := 0 to FWidth - 1 do
    begin
      r := Random(Amount);
      ScanLine[y][x] := ScanLine
        [TrimInt(y + (r - Random(r * 2)), 0, FHeight - 1)]
        [TrimInt(x + (r - Random(r * 2)), 0, FWidth - 1)];
    end;
end;

// ¸´ÖÆ
procedure TCnFontMask.CopyTo(Dst: TCnFontMask);
begin
  Dst.SetSize(FWidth, FHeight);
  Move(FBuff^, Dst.Buff^, FRowInc * FHeight);
end;

// ÊÍ·Å
destructor TCnFontMask.Destroy;
begin
  SetSize(0, 0);
  inherited;
end;

// ÂÖÀª»¯£¨SobelËã×Ó±ßÔµ¼ì²â£©
procedure TCnFontMask.Outline;
var
  x, y: Integer;
  s1, s2, s3, s4, Sum: Integer;
  Tmp: TCnFontMask;
  pDst: PByteArray;
  pUp, pMiddle, pDown: PByteArray; //¾í»ıÓÃÖ¸Õë
begin
  Tmp := TCnFontMask.Create;
  try
    CopyTo(Tmp);
    for y := 1 to Height - 2 do
    begin
      pUp := Tmp.ScanLine[y - 1];
      pMiddle := Tmp.ScanLine[y];
      pDown := Tmp.ScanLine[y + 1];
      pDst := ScanLine[y];
      for x := 1 to Width - 2 do
      begin
        s1 := Abs(pDown^[x] - pUp^[x]);
        s2 := Abs(pMiddle^[x + 1] - pMiddle^[x - 1]);
        s3 := Abs(pDown^[x - 1] - pUp^[x + 1]);
        s4 := Abs(pDown^[x + 1] - pUp^[x - 1]);
        Sum := (s1 + s2 + s3 + s4) shr 2;
        if Sum > 255 then
          pDst^[x] := 255
        else
          pDst^[x] := Sum;
      end;
    end;
  finally
    Tmp.Free;
  end;
end;

// É¨ÃèÏß
function TCnFontMask.GetScanLine(Row: Integer): PByteArray;
begin
  Result := PByteArray(TCnNativeInt(FBuff) + Row * FRowInc);
end;

// ÉèÖÃ´óĞ¡
procedure TCnFontMask.SetSize(AWidth, AHeight: Integer);
begin
  if (AWidth = FWidth) and (AHeight = FHeight) then Exit;
  if AWidth < 0 then AWidth := 0;
  if AHeight < 0 then AHeight := 0;
  FWidth := AWidth;
  FHeight := AHeight;
  FRowInc := (AWidth + 3) div 4 * 4;
  ReallocMem(FBuff, FRowInc * FHeight);
end;

//--------------------------------------------------------//
// ¿ìËÙÍ¼Ïñ´¦ÀíÀà                                         //
//--------------------------------------------------------//

{ TCnCanvas }

// ³õÊ¼»¯
constructor TCnCanvas.Create(ABitmap: TCnBitmap);
begin
  inherited Create;
  FDC := 0;
  FBitmap := ABitmap;
end;

// ÊÍ·Å
destructor TCnCanvas.Destroy;
begin
  FreeContext;
  inherited;
end;

// È¡ DC
function TCnCanvas.GetHandle: HDC;
begin
  Result := inherited Handle; // È¡¼Ì³Ğ¶øÀ´µÄ¾ä±ú
end;

// ´´½¨ DC£¬ÖØÔØ·½·¨£¬ÓÉ TCanvas ÄÚ²¿µ÷ÓÃ
procedure TCnCanvas.CreateHandle;
var
  H: HDC;
begin
  Lock;
  try
    FBitmap.HandleNeeded;     // ·ÖÅäÎ»Í¼¾ä±ú
    H := CreateCompatibleDC(0); // ´´½¨¼æÈİDC
    if H = 0 then raise ECnGraphics.Create(SCreateDCFail);
    if SelectObject(H, FBitmap.FHandle) = 0 then // Ñ¡ÔñÎ»Í¼µ½ÄÚ²¿DC
      raise ECnGraphics.Create(SSelectBmpToDCFail);
    FDC := H;
    inherited Handle := H;    // ÉèÖÃ¼Ì³Ğ¶øÀ´µÄ¾ä±ú
    CnCanvasList.Add(Self);   // Ôö¼Ó×ÔÉíµ½ÁĞ±íÖĞ
  finally
    Unlock;
  end;
end;

// ÊÍ·ÅDC
procedure TCnCanvas.FreeContext;
begin
  if FDC <> 0 then
  begin
    Lock;
    try
      inherited Handle := 0;  // ÊÍ·Å¼Ì³Ğ¶øÀ´µÄ¾ä±ú
      DeleteDC(FDC);          // É¾³ıDC
      FDC := 0;
      CnCanvasList.Remove(Self); // ´ÓÁĞ±íÖĞÉ¾³ı
    finally
      Unlock;
    end;
  end;
end;

{ TCnBitmap }

// ³õÊ¼»¯
constructor TCnBitmap.Create;
begin
  inherited;
  FSmoothFilter := True;
  FTransparent := False;
  FTransparentColor := clDefault;
  FGdiAllocStyle := DefGdiAllocStyle;
  FPenColor := clBlack;
  FPenWeight := pwNormal;
  FFontClear := False;
  FFontBkColor := clWhite;
end;

// ÊÍ·Å
destructor TCnBitmap.Destroy;
begin
  Lock;                       // ¶àÏß³ÌÍ¬²½
  try
    CleanUp;
    if FFont <> nil then FFont.Free;
    if FCanvas <> nil then FCanvas.Free;
    FreeGrayBmp;
  finally
    Unlock;
  end;
  inherited;
end;

//--------------------------------------------------------//
// ¶¯Ì¬Î»Í¼×ÊÔ´¡¢DC´¦Àí²¿·Ö                               //
// Ëã·¨Éè¼Æ£ºÖÜ¾¢Óğ                                       //
// Ëã·¨²Î¿¼£ºGraphic32¡¢pnBitmap¡¢Graphic.pas             //
//--------------------------------------------------------//

// ÊÍ·ÅËùÓĞÄÚ´æDC
procedure FreeBmpDC;
var
  I: Integer;
  Canvas: TCnCanvas;
begin
  with CnCanvasList.LockList do // Èç¹ûÆäËüÏß³ÌÒ²ÔÚ·ÃÎÊÔòµÈ´ı
  try
    for I := Count - 1 downto 0 do
    begin
      Canvas := TCnCanvas(Items[I]);
      if Canvas.TryLock then
      try
        Canvas.FreeContext;   // ÊÍ·ÅDC
      finally
        Canvas.Unlock;
      end;
    end;
  finally
    CnCanvasList.UnlockList;
  end;
end;

// ÊÍ·ÅÎ»Í¼¾ä±ú
procedure FreeBmpHandle(All: Boolean);
var
  I: Integer;
  Bmp: TCnBitmap;
begin
  with BitmapList.LockList do // Èç¹ûÆäËüÏß³ÌÒ²ÔÚ·ÃÎÊÔòµÈ´ı
  try
    for I := Count - 1 downto 0 do
    begin
      Bmp := TCnBitmap(Items[I]);
      if (All or (Bmp.GdiAllocStyle = gsInternal) or (Bmp.GdiAllocStyle =
        gsNormal) and (GetTickCount - Bmp.FGdiLastAccess > FreeGdiWaitTime))
        and Bmp.HandleAllocated then
      begin
        if Bmp.TryLock then
        try
          Bmp.HandleRelease;  // ÊÍ·ÅÎ»Í¼¾ä±ú
        finally
          Bmp.Unlock;
        end;
      end;
    end;
  finally
    BitmapList.UnlockList;
  end;
end;

// Î»Í¼×ÊÔ´¶¨Ê±¹ÜÀí£¨Àà·½·¨£©
// TTimer.OnTimerÒªÇóÒ»¸ö¶ÔÏó·½·¨£¬¹Ê¶¨Òå¸ÃÀà·½·¨
class procedure TCnBitmap.OnGdiActTimer(Sender: TObject);
begin
  FreeBmpDC;
  FreeBmpHandle(False);
end;

// È¡Canvas
function TCnBitmap.GetCanvas: TCnCanvas;
begin
  if FCanvas = nil then       // µÚÒ»´Î·ÃÎÊÊ±´´½¨
  begin
    FCanvas := TCnCanvas.Create(Self);
    FCanvas.OnChange := OnChildChange;
    FCanvas.OnChanging := OnChildChanging;
  end;
  FGdiLastAccess := GetTickCount; // ×îºó·ÃÎÊ»­²¼µÄÊ±¼ä
  Result := FCanvas;
end;

// ÊÍ·Å¾ä±ú¡¢×ÊÔ´£¬Çå±äÁ¿£¨±£»¤·½·¨£©
procedure TCnBitmap.CleanUp;
begin
  if HandleAllocated then     // ÒÑ·ÖÅäÎ»Í¼¾ä±ú
    HandleRelease(False)      // ÊÍ·ÅÎ»Í¼×ÊÔ´
  else
  begin
    if Assigned(FBits) then FreeMem(FBits); // ÊÍ·ÅÎ»Í¼Êı¾İ¿é
    if Assigned(FScanLine) then FreeMem(FScanLine); // É¾³ıÉ¨ÃèÏßÊı×é
    FBits := nil;
    FScanLine := nil;
    FWidth := 0;
    FHeight := 0;
    FSize := 0;
    FRowInc := 0;
    FGap := 0;
    FPenPosF.x := 0;
    FPenPosF.y := 0;
  end;
end;

// ÊÍ·Å¾ä±ú¡¢Çå±äÁ¿
procedure TCnBitmap.FreeImage;
begin
  CleanUp;
  Changed;
end;

// ¸üĞÂÉ¨ÃèÏßÖ¸ÕëÊı×é
procedure TCnBitmap.UpdateScanLine;
var
  I: Integer;
  x: TCnNativeInt;
begin
  ReallocMem(FScanLine, FHeight * SizeOf(PCnLine)); // ÖØĞÂ·ÖÅäÉ¨ÃèÏßÖ¸ÕëÊı×é¿Õ¼ä
  x := TCnNativeInt(FBits);
  for I := 0 to Height - 1 do
  begin
    FScanLine[I] := Pointer(x); // ³õÊ¼»¯É¨ÃèÏßÖ¸ÕëÊı×éÄÚÈİ
    Inc(x, FRowInc);
  end;
end;

// GDI ¾ä±úÒÑ·ÖÅä
function TCnBitmap.HandleAllocated: Boolean;
begin
  Result := FHandle <> 0;
end;

// ĞèÒª HBITMAP ¾ä±ú
procedure TCnBitmap.HandleNeeded;
var
  Tmp: Pointer;
begin
  if HandleAllocated then Exit;
  if Empty then               // ²»ÄÜÎª¿ÕÎ»Í¼·ÖÅä¾ä±ú
    raise EBitmapIsEmpty.Create(SCreateDCFromEmptyBmp);

  Lock;
  try
    FillChar(FBitmapInfo, SizeOf(TBitmapInfo), 0);
    with FBitmapInfo.bmiHeader do // ³õÊ¼»¯FBitmapInfo
    begin
      biSize := SizeOf(TBitmapInfoHeader);
      biWidth := Width;
      biHeight := -Height;    // ¸ß¶ÈÎª¸º£¬É¨ÃèÏß°´µÍµØÖ·µ½¸ßµØÖ··½Ê½´æ·Å
      biPlanes := 1;
      biBitCount := 24;       // 24 Bit RGB Î»Í¼
      biCompression := BI_RGB;
    end;                      // Ô­Í¼ÏñÊı¾İ
    Tmp := FBits;             // ´´½¨Î»Í¼
    FHandle := CreateDIBSection(0, FBitmapInfo, DIB_RGB_COLORS, FBits, 0, 0);
    if FHandle = 0 then       // ÎŞ·¨´´½¨Î»Í¼¾ä±ú´í
      raise ECnGraphics.Create(SAllocDIBFail);
    Move(Tmp^, FBits^, Size); // ÒÆ¶¯Ô­Êı¾İµ½ĞÂÊı¾İÇø
    FreeMem(Tmp);             // ÊÍ·ÅÔ­Êı¾İ¿é
    UpdateScanLine;           // ¸üĞÂÉ¨ÃèÏßÖ¸ÕëÊı×éÄÚÈİ
    BitmapList.Add(Self);     // Ôö¼Óµ½Î»Í¼ÁĞ±í
  finally
    Unlock;
  end;
end;

// ÊÍ·ÅHBITMAPÎ»Í¼¾ä±ú£¨²ÎÊıÎªTrue½«±£ÁôÎ»Í¼Êı¾İ£©
procedure TCnBitmap.HandleRelease(KeepData: Boolean);
var
  Tmp: Pointer;
begin
  if not HandleAllocated then Exit;

  if Assigned(FCanvas) then
    TCnCanvas(FCanvas).FreeContext; // ÊÍ·Å DC
  Lock;
  try
    if KeepData then          // ±£ÁôÊı¾İ£¬½öÉ¾³ıÎ»Í¼¾ä±ú
    begin
      Tmp := FBits;
      GetMem(FBits, FSize);
      Move(Tmp^, FBits^, FSize);
      UpdateScanLine;         // ¸üĞÂÉ¨ÃèÏßÖ¸ÕëÊı×éÄÚÈİ
    end else
    begin                     // Í¬Ê±ÊÍ·ÅÎ»Í¼Êı¾İ
      if Assigned(FScanLine) then FreeMem(FScanLine); // É¾³ıÉ¨ÃèÏßÊı×é
      FBits := nil;
      FScanLine := nil;
      FWidth := 0;
      FHeight := 0;
      FSize := 0;
      FRowInc := 0;
      FGap := 0;
      FPenPosF.x := 0;
      FPenPosF.y := 0;
    end;
    DeleteObject(FHandle);    // É¾³ıÎ»Í¼¶ÔÏó
    FHandle := 0;
    BitmapList.Remove(Self);  // ´ÓÎ»Í¼ÁĞ±íÖĞÉ¾³ı
  finally
    Unlock;
  end;
end;

// È¡Î»Í¼¾ä±ú HBITMAP
function TCnBitmap.GetHandle: HBITMAP;
begin
  if not HandleAllocated then HandleNeeded;
  Result := FHandle;
end;

// È¡µÃ DC
function TCnBitmap.GetDC: HDC;
begin
  Result := Canvas.Handle;
end;

// È¡Í¼Ïñ¿Õ×´Ì¬
function TCnBitmap.GetEmpty: Boolean;
begin
  Result := FBits = nil;
end;

// ÉèÖÃÍ¼Ïñ³ß´ç
procedure TCnBitmap.SetSize(AWidth, AHeight: Integer);
begin
  if AWidth < 0 then AWidth := 0;
  if AHeight < 0 then AHeight := 0;
  if (AWidth = Width) and (AHeight = Height) then Exit;

  Changing;
  CleanUp;                    // ÊÍ·ÅÔ­ÄÚÈİ
  FWidth := AWidth;
  FHeight := AHeight;
  if (AWidth > 0) and (AHeight > 0) then
  begin
    FRowInc := (FWidth * 3) + FWidth mod 4; // Ã¿ĞĞÉ¨ÃèÏß³¤¶È£¬°´ 4 ×Ö½Ú¶ÔÆë
    FGap := FWidth mod 4;       // Ã¿ĞĞÉ¨ÃèÏß×îºóµÄÎŞĞ§×ÖÊı
    FSize := FRowInc * FHeight; // Í¼Ïñ´óĞ¡
    GetMem(FBits, FSize);       // ·ÖÅäÍ¼Ïñ¿Õ¼ä
    UpdateScanLine;             // ¸üĞÂÉ¨ÃèÏßÖ¸ÕëÊı×éÄÚÈİ
    //Fill(CnWinColor(GetTranColor));  // ÒÔÍ¸Ã÷É«Ìî³ä
  end;
  Changed;
end;

// ÉèÖÃÍ¼Ïñ³ß´ç£¨TRect ÀàĞÍ²ÎÊı£©
procedure TCnBitmap.SetSize(ARect: TRect);
begin
  SetSize(ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);
end;

// ÉèÖÃ¸ß¶È
procedure TCnBitmap.SetHeight(const Value: Integer);
begin
  if FHeight <> Value then SetSize(FWidth, Value);
end;

// ÉèÖÃ¿í¶È
procedure TCnBitmap.SetWidth(const Value: Integer);
begin
  if FWidth <> Value then SetSize(Value, FHeight);
end;

//--------------------------------------------------------//
// ¸¨Öú´úÂë                                               //
//--------------------------------------------------------//

// Êı¾İÑ¹Ëõ£¬·µ»ØÄ¿±êÊı¾İ³¤¶È
function TCnBitmap.Compress(var pData: Pointer; ASize: Integer): Integer;
begin
  Result := ASize;            { TODO -oÖÜ¾¢Óğ -cÑ¹Ëõ½âÑ¹ : Î»Í¼Êı¾İÑ¹Ëõ½âÑ¹¡£ }
end;

// Êı¾İ½âÑ¹£¬·µ»ØÄ¿±êÊı¾İ³¤¶È£¨³ö´í·µ»Ø-1£©
function TCnBitmap.DeCompress(var pData: Pointer; ASize: Integer): Integer;
begin
  Result := ASize;
end;

// È¡»æÖÆ¾ØĞÎÇø
function TCnBitmap.GetClientRect: TRect;
begin
  Result := Rect(0, 0, Width, Height);
end;

// È¡Í¸Ã÷É«
function TCnBitmap.GetTranColor: TCnColor;
begin
  if TransparentColor <> clDefault then
    Result := CnColor(TransparentColor)
  else if not Empty then
    Result := Pixels[0, Height - 1] // Ä¬ÈÏÎª×óÏÂ½ÇÏñËØ£¨Óë TBitmap ±£³ÖÒ»ÖÂ£©
  else
    Result := CnColor(0, 0, 0);
end;

// È¡Æ½»¬×ÖÌå
function TCnBitmap.GetFont: TCnFont;
begin
  if FFont = nil then
  begin
    FFont := TCnFont.Create;
    FFont.OnChange := OnChildChange;
  end;
  Result := FFont;
end;

// ÉèÖÃÆ½»¬×ÖÌå
procedure TCnBitmap.SetFont(const Value: TCnFont);
begin
  if Value <> nil then
    Font.Assign(Value);       // ×Ô¶¯µ÷ÓÃ GetFont ·½·¨±£Ö¤ÊµÀı»¯
end;

// ÒÔÖ¸¶¨É«Ìî³äÎ»Í¼
procedure TCnBitmap.Fill(Color: TColor);
begin
  FillRect(ClientRect, Color);
end;

// ÒÔÖ¸¶¨É«Ìî³ä¾ØĞÎÇøÓò
// Ëã·¨£ºÖÜ¾¢Óğ
procedure TCnBitmap.FillRect(Rect: TRect; Color: TColor);
var
  x, y, w, h, I, lw: Integer;
  ARect: TRect;
  Tmp: PCnColor;
  ARGB: TCnColor;
begin
  if Empty then
    Exit;         // ÎŞÓĞĞ§ÇøÓò
  if not IntersectRect(ARect, Rect, ClientRect) then Exit;
  Changing;
  if (Color = clBlack) and RectEqu(ARect, ClientRect) then
    FillChar(Bits^, Size, 0)  // È«ÇøÓòÇåÁã
  else
  begin
    DeRect(ARect, x, y, w, h);
    lw := w * 3;              // ´æ´¢¿í¶È
    ARGB := CnColor(Color);   // ×ª»»Îª RGB ¸ñÊ½
    Tmp := @FScanLine[y][x];
    if Color = clBlack then   // ºÚÉ«È«²¿ÇåÁã
      FillChar(Tmp^, lw, 0)
    else
    begin
      for I := 0 to w - 1 do  // Ìî³äµÚÒ»ĞĞÉ¨ÃèÏß
      begin
        Tmp^ := ARGB;
        Inc(Tmp);
      end;
    end;
    Tmp := @FScanLine[y][x];
    for I := y + 1 to y + h - 1 do // ¸´ÖÆµ½ÆäËüÉ¨ÃèÏß
      Move(Tmp^, (@FScanLine[I][x])^, lw);
  end;
  Changed;
end;

// ÒÔÖ¸¶¨É«»­Ïß
procedure TCnBitmap.DrawLine(x1, y1, x2, y2: Integer; Color: TColor);
var
  x, y: Integer;
  l, r, t, b: Integer;
  RGB: TCnColor;
begin
  if Empty then Exit;
  if (InBound(x1, 0, Width - 1) or InBound(x2, 0, Width - 1)) and
    (InBound(y1, 0, Height - 1) or InBound(y2, 0, Height - 1)) then
  begin
    RGB := CnColor(Color);
    l := TrimInt(Min(x1, x2), 0, Width - 1);
    r := TrimInt(Max(x1, x2), 0, Width - 1);
    t := TrimInt(Min(y1, y2), 0, Height - 1);
    b := TrimInt(Max(y1, y2), 0, Height - 1);
    if x1 = x2 then
      for y := t to b do
        FScanLine[y][x1] := RGB
    else if y1 = y2 then
      for x := l to r do
        FScanLine[y1][x] := RGB
    else if Abs(x1 - x2) > Abs(y1 - y2) then
      for x := l to r do
        FScanLine[Round((x - x1) / (x2 - x1) * (y2 - y1)) + y1][x] := RGB
    else
      for y := t to b do
        FScanLine[y][Round((y - y1) / (y2 - y1) * (x2 - x1)) + x1] := RGB;
  end;
end;

// ÒÔÖ¸¶¨É«»­¾ØĞÎ
procedure TCnBitmap.FrameRect(Rect: TRect; Color: TColor);
begin
  if Empty then
    Exit;

  with Rect do
  begin
    DrawLine(Left, Top, Left, Bottom, Color);
    DrawLine(Left, Bottom, Right, Bottom, Color);
    DrawLine(Right, Bottom, Right, Top, Color);
    DrawLine(Right, Top, Left, Top, Color);
  end;
end;

// ÒÔµ±Ç°Í¼Ïñ´´½¨Ò»¸ö Region
function TCnBitmap.CreateRegion(var RgnData: PRgnData): Integer;
const
  Max = 10000;
var
  x, y, Tmp: Integer;
  Rts: array[0..Max] of TRect;
  Count: Integer;
  TranColor, c: TCnColor;
begin
  Result := 0;
  if Empty then
    Exit;

  Count := 0;
  TranColor := GetTranColor;
  for y := 0 to Height - 1 do
  begin
    x := 0;
    while x < Width do
    begin
      c := Pixels[x, y];
      if not CnColorEqu(c, TranColor) then
      begin
        Tmp := x;
        c := Pixels[Tmp, y];
        while not CnColorEqu(c, TranColor) do
        begin
          Inc(Tmp);
          c := Pixels[Tmp, y];
          if Tmp >= Width then Break;
        end;
        Rts[Count] := Rect(x, y, Tmp, y + 1);
        Inc(Count);
        x := Tmp;
        Continue;
      end;
      Inc(x);
    end;
  end;
  // ´´½¨ Region Êı¾İ
  Result := Count * SizeOf(TRect);
  GetMem(Rgndata, SizeOf(TRgnDataHeader) + Result);
  FillChar(Rgndata^, SizeOf(TRgnDataHeader) + Result, 0);
  RgnData^.rdh.dwSize := SizeOf(TRgnDataHeader);
  RgnData^.rdh.iType := RDH_RECTANGLES;
  RgnData^.rdh.nCount := Count;
  RgnData^.rdh.nRgnSize := 0;
  RgnData^.rdh.rcBound := Rect(0, 0, Width, Height);
  // ¸üĞÂRegion
  Move(Rts, RgnData^.Buffer, Result);
  Result := SizeOf(TRgnDataHeader) + Count * SizeOf(TRect);
end;

//--------------------------------------------------------//
// ¸³Öµ²Ù×÷                                               //
// Ëã·¨À´Ô´£ºGraphic32¡¢pnBitmap                          //
// Ëã·¨ĞŞ¸Ä£ºÖÜ¾¢Óğ                                       //
//--------------------------------------------------------//

// ¸³Öµ²Ù×÷
procedure TCnBitmap.Assign(Source: TPersistent);
begin
  BeginUpdate;
  try
    if Source = nil then      // ¸³¿ÕÖµÖÃ¿Õ
    begin
      SetSize(0, 0);
      Exit;
    end
    else if Source is TCnBitmap then
    begin
      SetSize(TCnBitmap(Source).Width, TCnBitmap(Source).Height);
      CopyMemory(Bits, TCnBitmap(Source).Bits, Size);
      FSmoothFilter := TCnBitmap(Source).FSmoothFilter;
      FTransparentColor := TCnBitmap(Source).FTransparentColor;
      FTransparent := TCnBitmap(Source).FTransparent;
      Exit;
    end
    else if Source is TBitmap then
    begin
      SetSize(TBitmap(Source).Width, TBitmap(Source).Height);
      if not Empty then
        Bitblt(DC, 0, 0, Width, Height, TBitmap(Source).Canvas.Handle, 0, 0, SRCCOPY);
      FTransparentColor := TBitmap(Source).TransparentColor;
      FTransparent := TBitmap(Source).Transparent;
      Exit;
    end
    else if Source is TGraphic then // TIcon¡¢TJpegImage µÈµÈ
    begin
      SetSize(TGraphic(Source).Width, TGraphic(Source).Height);
      if not Empty then
      begin
        Fill(CnWinColor(GetTranColor));
        TGraphicAccess(Source).Draw(Canvas, Rect(0, 0, Width, Height));
      end;
      FTransparent := TGraphicAccess(Source).Transparent;
      Exit;
    end
    else if Source is TPicture then
    begin
      Assign(TPicture(Source).Graphic);
      Exit;
    end
    else
      inherited;
  finally
    if GdiAllocStyle = gsInternal then HandleRelease; // ÊÍ·Å¾ä±ú
    EndUpdate;
  end;
end;

// ¸³Öµµ½Ä¿±ê£¨±£»¤·½·¨£©
procedure TCnBitmap.AssignTo(Dest: TPersistent);
var
  Bmp: TBitmap;
begin
  try
    if Dest is TBitmap then
    begin
      TBitmap(Dest).HandleType := bmDIB;
      TBitmap(Dest).PixelFormat := pf24Bit;
      TBitmap(Dest).Width := Width;
      TBitmap(Dest).Height := Height;
      TBitmap(Dest).TransparentColor := FTransparentColor;
      TBitmap(Dest).Transparent := FTransparent;
      if not Empty then
        Bitblt(TBitmap(Dest).Canvas.Handle, 0, 0, Width, Height, DC, 0, 0, SRCCOPY);
      Exit;
    end
    else if Dest is TGraphic then // TIcon¡¢TJpegImage µÈµÈ
    begin
      Bmp := TBitmap.Create;
      try
        AssignTo(Bmp);
        Dest.Assign(Bmp);
      finally
        Bmp.Free;
      end;
      Exit;
    end
    else if Dest is TPicture then
    begin
      AssignTo(TPicture(Dest).Graphic);
      Exit;
    end
    else
      inherited;
  finally
    if GdiAllocStyle = gsInternal then HandleRelease; //ÊÍ·Å¾ä±ú
  end;
end;

//--------------------------------------------------------//
// ´æ´¢Êı¾İÊôĞÔ                                           //
// Ëã·¨À´Ô´£ºDelphi Graphic.pas                           //
// Ëã·¨ĞŞ¸Ä£ºÖÜ¾¢Óğ                                       //
//--------------------------------------------------------//

// ¶¨Òå´æ´¢ÊôĞÔ£¨±£´æÔÚ DFM ÖĞµÄ×Ô¶¨ÒåÊôĞÔ£©
procedure TCnBitmap.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  begin
    if Filer.Ancestor <> nil then // ÊÇ´Ó»ù´°Ìå¼Ì³Ğ¶øÀ´
      Result := not (Filer.Ancestor is TCnBitmap) or
        not Equals(TCnBitmap(Filer.Ancestor))
    else
      Result := not Empty;
  end;
begin                         // ¶¨ÒåÊôĞÔ Data ±£´æÍ¼ÏñÊı¾İ
  Filer.DefineBinaryProperty('Data', ReadData, WriteData, DoWrite);
end;

// ´Ó DFM Á÷ÖĞ¶ÁÊı¾İ¹ı³Ì
procedure TCnBitmap.ReadData(Stream: TStream);
var
  AWidth, AHeight, ASize: Integer;
  Buff: Pointer;
begin
  Stream.Read(AWidth, SizeOf(AWidth));
  Stream.Read(AHeight, SizeOf(AHeight));
  Stream.Read(ASize, SizeOf(ASize));
  if ASize = 0 then Exit;
  GetMem(Buff, ASize);
  try
    Stream.Read(Buff^, ASize);
    if DeCompress(Buff, ASize) > 0 then // Êı¾İ½âÑ¹
      LoadFromMemory(Buff, AWidth, AHeight)
    else
      raise ECnGraphics.Create(SReadBmpError);
  finally
    FreeMem(Buff);
  end;
end;

// Ğ´Êı¾İµ½ DFM Á÷¹ı³Ì
procedure TCnBitmap.WriteData(Stream: TStream);
var
  ASize: Integer;
  Buff: Pointer;
begin
  Stream.Write(Width, SizeOf(Width));
  Stream.Write(Height, SizeOf(Height));
  if Size = 0 then
    Stream.Write(Size, SizeOf(Size))
  else
  begin
    GetMem(Buff, Size);
    try
      Move(Bits^, Buff^, Size);      // ÁÙÊ±»º³å
      ASize := Compress(Buff, Size); // Ñ¹ËõÊı¾İ
      Stream.Write(ASize, SizeOf(ASize));
      Stream.Write(Buff^, ASize);
    finally
      FreeMem(Buff);
    end;
  end;
end;

// ÅĞ¶ÏÁ½¸öÍ¼ÏñÊı¾İÊÇ·ñÈ«µÈ
function TCnBitmap.Equals(Obj: TObject): Boolean;
var
  MyImage, BmpImage: TMemoryStream;
  Bitmap: TCnBitmap;
begin
  Bitmap := TCnBitmap(Obj);
  Result := (Bitmap <> nil) and (ClassType = Bitmap.ClassType);
  if Empty or Bitmap.Empty then
  begin
    Result := Empty and Bitmap.Empty;
    Exit;
  end;
  if Result then
  begin
    MyImage := TMemoryStream.Create;
    try
      WriteData(MyImage);
      BmpImage := TMemoryStream.Create;
      try
        Bitmap.WriteData(BmpImage);
        Result := (MyImage.Size = BmpImage.Size) and
          CompareMem(MyImage.memory, BmpImage.memory, MyImage.Size);
      finally
        BmpImage.Free;
      end;
    finally
      MyImage.Free;
    end;
  end;
end;

{$WARNINGS ON}

//-------------------------------------------------------//
// ÏñËØ·ÃÎÊÓÃ´úÂë                                         //
// Ëã·¨Éè¼Æ£ºÖÜ¾¢Óğ                                       //
//--------------------------------------------------------//

// È¡ÏñËØÑÕÉ«Öµ
function TCnBitmap.GetPixel(x, y: Integer): TCnColor;
begin
  if Empty then
    raise EBitmapIsEmpty.Create(SBitmapIsEmpty);
  if (x < 0) or (x > Width - 1) or (y < 0) or (y > Height - 1) then
    raise EInvalidPixel.CreateFmt(SInvalidPixel, [x, y])
  else
    Result := FScanLine[y, x];
end;

// Ğ´ÏñËØ
procedure TCnBitmap.SetPixel(x, y: Integer; const Value: TCnColor);
begin
  if Empty then
    raise EBitmapIsEmpty.Create(SBitmapIsEmpty);
  if (x < 0) or (x > Width - 1) or (y < 0) or (y > Height - 1) then
    raise EInvalidPixel.CreateFmt(SInvalidPixel, [x, y])
  else
    FScanLine[y, x] := Value;
end;

// È¡É¨ÃèÏß
function TCnBitmap.GetScanLine(Row: Integer): PCnLine;
begin
  if Empty then
    raise EBitmapIsEmpty.Create(SBitmapIsEmpty);
  if (Row < 0) or (Row > Height - 1) then
    raise EInvalidScanLine.CreateFmt(SInvalidScanLine, [Row])
  else
    Result := FScanLine[Row];
end;

//--------------------------------------------------------//
// ÓëÍâ²¿Êı¾İ½»»»                                         //
// Ëã·¨Éè¼Æ£ºÖÜ¾¢Óğ                                       //
// Ëã·¨²Î¿¼£ºGraphic32¡¢pnBitmap                          //
//--------------------------------------------------------//

// ÉèÖÃ¿ÕÎ»Í¼
procedure TCnBitmap.LoadBlank(AWidth, AHeight: Integer);
begin
  SetSize(AWidth, AHeight);
end;

// ÄÚ´æÖĞ×°ÔØÎ»Í¼£¨RGB Êı¾İ¿é£©
procedure TCnBitmap.LoadFromMemory(ABits: Pointer; AWidth, AHeight: Integer);
begin
  Changing;
  SetSize(AWidth, AHeight);   // ÉèÖÃÎ»Í¼³ß´ç
  if not Empty then
    Move(ABits^, FBits^, FSize); // ¸´ÖÆÎ»Í¼Êı¾İ
  Changed;
end;

// ´ÓÁ÷ÖĞ×°ÔØÎ»Í¼
procedure TCnBitmap.LoadFromStream(Stream: TStream);
var
  Bmp: TBitmap;
begin
  Changing;
  Bmp := TBitmap.Create;
  try
    Bmp.LoadFromStream(Stream);
    Assign(Bmp);
  finally
    Bmp.Free;
  end;
  Changed;
end;

// ´ÓÎÄ¼şÖĞ×°ÔØÎ»Í¼£¨Í¨¹ı TPicture ×°ÔØ£¬Ö§³Ö BMP¡¢ICO¡¢JPEG µÈ¸ñÊ½£©
procedure TCnBitmap.LoadFromFile(const FileName: string);
var
  Picture: TPicture;
begin
  Changing;
  Picture := TPicture.Create;
  try
    Picture.LoadFromFile(FileName);
    Assign(Picture);
  finally
    Picture.Free;
  end;
  Changed;
end;

// ´Ó×ÊÔ´ÖĞ×°ÔØÎ»Í¼£¨×ÊÔ´ ID£©
procedure TCnBitmap.LoadFromResourceID(instance: THandle; ResID: Integer);
var
  Bmp: TBitmap;
begin
  Changing;
  Bmp := TBitmap.Create;
  try
    Bmp.LoadFromResourceID(instance, ResID);
    Assign(Bmp);
  finally
    Bmp.Free;
  end;
  Changed;
end;

// ´Ó×ÊÔ´ÖĞ×°ÔØÎ»Í¼£¨×ÊÔ´Ãû£©
procedure TCnBitmap.LoadFromResourceName(instance: THandle;
  const ResName: string);
var
  Bmp: TBitmap;
begin
  Changing;
  Bmp := TBitmap.Create;
  try
    Bmp.LoadFromResourceName(instance, ResName);
    Assign(Bmp);
  finally
    Bmp.Free;
  end;
  Changed;
end;

// ´Ó¼ôÌù°åÖĞ×°ÔØÎ»Í¼
procedure TCnBitmap.LoadFromClipboardFormat(AFormat: Word; AData: THandle;
  APalette: HPALETTE);
var
  Bmp: TBitmap;
begin
  Changing;
  Bmp := TBitmap.Create;
  try
    Bmp.LoadFromClipboardFormat(AFormat, AData, APalette);
    Assign(Bmp);
  finally
    Bmp.Free;
  end;
  Changed;
end;

// ±£´æÎ»Í¼µ½Á÷
procedure TCnBitmap.SaveToStream(Stream: TStream);
var
  Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  try
    AssignTo(Bmp);
    Bmp.SaveToStream(Stream);
  finally
    Bmp.Free;
  end;
end;

// ±£´æÎ»Í¼µ½ÎÄ¼ş
procedure TCnBitmap.SaveToFile(const FileName: string);
var
  Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  try
    AssignTo(Bmp);
    Bmp.SaveToFile(FileName);
  finally
    Bmp.Free;
  end;
end;

// ¸´ÖÆÎ»Í¼µ½¼ôÌù°åÖĞ
procedure TCnBitmap.SaveToClipboardFormat(var Format: Word;
  var Data: THandle; var APalette: HPALETTE);
var
  Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  try
    AssignTo(Bmp);
    Bmp.SaveToClipboardFormat(Format, Data, APalette);
  finally
    Bmp.Free;
  end;
end;

//--------------------------------------------------------//
// Í¼Ïñ»æÖÆ¹ı³Ì                                           //
// Ô­Ê¼Ëã·¨£ºFastLib                                      //
// Ëã·¨ĞŞ¸Ä£ºÖÜ¾¢Óğ£¨Ôö¼ÓÍ¸Ã÷»æÖÆ¹¦ÄÜ¡¢À©Õ¹ºÍ²¿·ÖĞŞÕı£© //
//--------------------------------------------------------//

// ¼ÆËãÁ½Î»Í¼Ïà½»Î»ÖÃ
function TCnBitmap.CalcDrawRect(DstX, DstY: Integer; SrcRect,
  SrcClientRect: TRect; var dx, dy, sx, sy, w, h: Integer): Boolean;
begin
  dx := DstX;
  dy := DstY;
  DeRect(SrcRect, sx, sy, w, h);
  if dx < 0 then              // Ä¿±ê×ø±ê³¬³ö·¶Î§
  begin
    Dec(sx, dx);
    Inc(w, dx);
    dx := 0;
  end;
  if dy < 0 then
  begin
    Dec(sy, dy);
    Inc(h, dy);
    dy := 0;
  end;
  if sx < 0 then              // Ô´×ø±ê³¬³ö·¶Î§
  begin
    Dec(dx, sx);
    Inc(w, sx);
    sx := 0;
  end;
  if sy < 0 then
  begin
    Dec(dy, sy);
    Inc(h, sy);
    sy := 0;
  end;
  Result := (sx < SrcClientRect.Right) and (sy < SrcClientRect.Bottom);
  if Result then
  begin
    if sx + w > RectWidth(SrcClientRect) then
      Dec(w, sx + w - RectWidth(SrcClientRect));
    if sy + h > RectHeight(SrcClientRect) then
      Dec(h, sy + h - RectHeight(SrcClientRect));
    if dx + w > Width then Dec(w, dx + w - Width);
    if dy + h > Height then Dec(h, dy + h - Height);
    Result := (w > 0) and (h > 0);
  end;
end;

// »æÖÆ TCnBitmap Î»Í¼ÔöÇ¿°æ
procedure TCnBitmap.DoDraw(DstX, DstY: Integer; Src: TCnBitmap;
  SrcRect: TRect; Tran: Boolean);
var
  n1, n2: Pointer;
  I, J: Integer;
  p1, p2: PCnColor;
  x, y, sx, sy, w, h: Integer;
  TranColor: TCnColor;
begin
  if Empty or not Assigned(Src) or Src.Empty or not CalcDrawRect(DstX, DstY,
    SrcRect, Src.ClientRect, x, y, sx, sy, w, h) then Exit;

  Changing;
  if Tran then                // Í¸Ã÷»æÖÆ
  begin
    TranColor := Src.GetTranColor;
    for I := 0 to h - 1 do
    begin
      p1 := @FScanLine[y + I][x];
      P2 := @Src.FScanLine[sy + I][sx];
      for J := 0 to w - 1 do
      begin                   // Í¸Ã÷ÅĞ¶Ï
        if (p2.b <> TranColor.b) or (p2.g <> TranColor.g) or (p2.r <> TranColor.r) then
          p1^ := p2^;
        Inc(p1);
        Inc(p2);
      end;
    end;
  end
  else
  begin
    n1 := @FScanLine[y][x];   // Ä¿±êÎ»Í¼×óÉÏ½ÇÏñËØµØÖ·
    n2 := @Src.FScanLine[sy][sx]; // Ô´Î»Í¼×óÉÏ½ÇÏñËØµØÖ·
    for I := 0 to h - 1 do
    begin
      Move(n2^, n1^, w * 3);  // ¸´ÖÆÍ¼ÏñÊı¾İ
      n1 := Pointer(TCnNativeInt(n1) + RowInc); // Ôö³¤Ò»ĞĞÉ¨ÃèÏß
      n2 := Pointer(TCnNativeInt(n2) + Src.RowInc);
    end;
  end;
  Changed;
end;

// »æÖÆ TCnBitmap Î»Í¼
procedure TCnBitmap.Draw(DstX, DstY: Integer; Src: TCnBitmap);
begin
  if Empty or not Assigned(Src) or Src.Empty then Exit;
  DrawEx(DstX, DstY, Src, Src.ClientRect);
end;

// »æÖÆ TCnBitmap Î»Í¼ÔöÇ¿°æ
procedure TCnBitmap.DrawEx(DstX, DstY: Integer; Src: TCnBitmap;
  SrcRect: TRect);
begin
  if Empty or not Assigned(Src) or Src.Empty then Exit;
  DoDraw(DstX, DstY, Src, SrcRect, Src.Transparent);
end;

// »æÖÆ TGraphic Î»Í¼£¬TBitmap¡¢TIcon¡¢TJpegImage µÈ
procedure TCnBitmap.Draw(DstX, DstY: Integer; Src: TGraphic);
begin
  if Empty or not Assigned(Src) or Src.Empty then Exit;
  DrawEx(DstX, DstY, Src, Rect(0, 0, Src.Width, Src.Height));
end;

// »æÖÆ TGraphic Î»Í¼ÔöÇ¿°æ
procedure TCnBitmap.DrawEx(DstX, DstY: Integer; Src: TGraphic;
  SrcRect: TRect);
begin
  if Empty or not Assigned(Src) or Src.Empty then Exit;
  Changing;
  TGraphicAccess(Src).Draw(Canvas, Rect(DstX, DstY, DstX +
    SrcRect.Right - SrcRect.Left, DstY + SrcRect.Bottom - SrcRect.Top));
  if GdiAllocStyle = gsInternal then HandleRelease; //ÊÍ·Å¾ä±ú
  Changed;
end;

// ´Ó DC ÉÏ¸´ÖÆÎ»Í¼£¬±ØĞëÖ¸¶¨Ô´¾ØĞÎ
procedure TCnBitmap.Draw(DstX, DstY: Integer; hSrc: HDC; SrcRect: TRect);
begin
  if Empty then Exit;
  Changing;
  Bitblt(DC, DstX, DstY, SrcRect.Right - SrcRect.Left, SrcRect.Bottom -
    SrcRect.Top, hSrc, SrcRect.Left, SrcRect.Top, SRCCOPY);
  if GdiAllocStyle = gsInternal then HandleRelease; //ÊÍ·Å¾ä±ú
  Changed;
end;

// »æÖÆ×ÔÉíµ½ DC
procedure TCnBitmap.DrawTo(hDst: HDC; DstX, DstY: Integer);
begin
  DrawToEx(hDst, DstX, DstY, ClientRect);
end;

// »æÖÆ×ÔÉíµ½ DC ÔöÇ¿°æ
procedure TCnBitmap.DrawToEx(hDst: HDC; DstX, DstY: Integer; SrcRect: TRect);
var
  Bmp: TCnBitmap;
  x, y, w, h: Integer;
begin
  if Empty then Exit;
  try
    DeRect(SrcRect, x, y, w, h);
    if Transparent then
    begin
      Bmp := TCnBitmap.Create;
      try
        Bmp.SetSize(SrcRect);
        Bmp.Draw(0, 0, hDst, Bounds(DstX, DstY, w, h));
        Bmp.DrawEx(0, 0, Self, SrcRect);
        Bitblt(hDst, DstX, DstY, w, h, Bmp.DC, 0, 0, SRCCOPY);
      finally
        Bmp.Free;
      end;
    end
    else
      Bitblt(hDst, DstX, DstY, w, h, DC, x, y, SRCCOPY);
  finally
    if GdiAllocStyle = gsInternal then HandleRelease; //ÊÍ·Å¾ä±ú
  end;
end;

// È¡µÈ±ÈËõ·Å¾ØĞÎ
function TCnBitmap.GetResizeRect(Src: TRect): TRect;
var
  cx, cy: Single;
  w, h: Integer;
begin
  cx := Width / RectWidth(Src);
  cy := Height / RectHeight(Src);
  w := Round(Min(cx, cy) * RectWidth(Src));
  h := Round(Min(cx, cy) * RectHeight(Src));
  Result.Left := (Width - w) div 2;
  Result.Right := Result.Left + w;
  Result.Top := (Height - h) div 2;
  Result.Bottom := Result.Top + h;
end;

// °´Ö¸¶¨Ä£Ê½»æÖÆ
procedure TCnBitmap.DrawMode(Src: TCnBitmap; Mode: TCnDrawMode);
begin
  case Mode of
    dmDraw: Draw(0, 0, Src);
    dmCenter: CenterDraw(Src);
    dmStretched: StretchDraw(Src);
    dmTiled: TileDraw(Src);
    dmResize: StretchDrawEx(GetResizeRect(Src.ClientRect), Src.ClientRect, Src);
  end;
end;

// °´Ö¸¶¨Ä£Ê½»æÖÆ£¬Ö§³Ö Alpha »ìºÏ
procedure TCnBitmap.DrawModeEx(Src: TCnBitmap; Mode: TCnDrawMode;
  Alpha: TCnAlpha);
var
  Bmp: TCnBitmap;
  ARect: TRect;
begin
  if Empty or not Assigned(Src) or Src.Empty then Exit;
  case Mode of
    dmDraw: AlphaDraw(0, 0, Src, Src.ClientRect, Alpha);
    dmCenter: AlphaDraw((Width - Src.Width) div 2, (Height - Src.Height) div 2,
        Src, Src.ClientRect, Alpha);
    dmStretched: AlphaDraw(Src, Alpha, True);
    dmTiled:
      begin
        Bmp := TCnBitmap.Create; // ÁÙÊ±Î»Í¼
        try
          if Src.Transparent then
            Bmp.Assign(Self)  // Í¸Ã÷Ê±¸´ÖÆ×ÔÉí
          else
            Bmp.SetSize(ClientRect);
          Bmp.TileDraw(Src);
          AlphaDraw(0, 0, Bmp, Bmp.ClientRect, Alpha);
        finally
          Bmp.Free;
        end;
      end;
    dmResize:
      begin
        Bmp := TCnBitmap.Create; // ÁÙÊ±Î»Í¼
        try
          ARect := GetResizeRect(Src.ClientRect);
          Bmp.SetSize(ARect);
          if Src.Transparent then // Í¸Ã÷Ê±¸´ÖÆ×ÔÉí
            Bmp.DoDraw(0, 0, Self, ARect, False);
          Bmp.SmoothFilter := SmoothFilter;
          Bmp.StretchDraw(Src);
          AlphaDraw(ARect.Left, ARect.Top, Bmp, Bmp.ClientRect, Alpha);
        finally
          Bmp.Free;
        end;
      end;
  end;
end;

// °´Ö¸¶¨Ä£Ê½»æÖÆ
procedure TCnBitmap.DrawMode(Src: TGraphic; Mode: TCnDrawMode);
var
  ARect: TRect;
begin
  case Mode of
    dmDraw: Draw(0, 0, Src);
    dmCenter: CenterDraw(Src);
    dmStretched: StretchDraw(Src);
    dmTiled: TileDraw(Src);
    dmResize:
      begin
        ARect := Rect(0, 0, Src.Width, Src.Height);
        StretchDrawEx(GetResizeRect(ARect), ARect, Src);
      end;
  end;
end;

// °´Ö¸¶¨Ä£Ê½»æÖÆ£¬Ö§³Ö Alpha »ìºÏ
procedure TCnBitmap.DrawModeEx(Src: TGraphic; Mode: TCnDrawMode;
  Alpha: TCnAlpha);
var
  Bmp: TCnBitmap;
begin
  if Empty or not Assigned(Src) or Src.Empty then Exit;
  Bmp := TCnBitmap.Create;
  try
    Bmp.Assign(Src);
    DrawModeEx(Bmp, Mode, Alpha);
  finally
    Bmp.Free;
  end;
end;

//--------------------------------------------------------//
// Í¼ÏñÖĞĞÄ»æÖÆ¹ı³Ì                                       //
// Ëã·¨Éè¼Æ£ºÖÜ¾¢Óğ                                       //
//--------------------------------------------------------//

// »æÖÆ TCnBitmap Î»Í¼µ½ÖĞĞÄ
procedure TCnBitmap.CenterDraw(Src: TCnBitmap);
begin
  if Empty or not Assigned(Src) or Src.Empty then Exit;
  Draw((Width - Src.Width) div 2, (Height - Src.Height) div 2, Src);
end;

// »æÖÆ TGraphic µ½ÖĞĞÄ
procedure TCnBitmap.CenterDraw(Src: TGraphic);
begin
  if Empty or not Assigned(Src) or Src.Empty then Exit;
  Draw((Width - Src.Width) div 2, (Height - Src.Height) div 2, Src);
end;

//--------------------------------------------------------//
// Í¼ÏñÆ½ÆÌ»æÖÆ¹ı³Ì                                       //
// Ëã·¨Éè¼Æ£ºÖÜ¾¢Óğ                                       //
//--------------------------------------------------------//

// Æ½ÆÌ»æÖÆ TCnBitmap Î»Í¼
procedure TCnBitmap.TileDraw(Src: TCnBitmap);
begin
  TileDrawEx(ClientRect, Src);
end;

// Æ½ÆÌ»æÖÆ TCnBitmap Î»Í¼ÔöÇ¿°æ
procedure TCnBitmap.TileDrawEx(DstRect: TRect; Src: TCnBitmap);
var
  I, J, x, y, w, h: Integer;
begin
  if Empty or not Assigned(Src) or Src.Empty then Exit;
  BeginUpdate;
  try
    DeRect(DstRect, x, y, w, h);
    for I := 0 to w div Src.Width do
      for J := 0 to h div Src.Height do
        Draw(x + I * Src.Width, y + J * Src.Height, Src);
  finally
    EndUpdate;
  end;
end;

// Æ½ÆÌ»æÖÆ TGraphic
procedure TCnBitmap.TileDraw(Src: TGraphic);
begin
  TileDrawEx(ClientRect, Src);
end;

// Æ½ÆÌ»æÖÆ TGraphic ÔöÇ¿°æ
procedure TCnBitmap.TileDrawEx(DstRect: TRect; Src: TGraphic);
var
  I, J, x, y, w, h: Integer;
begin
  if Empty or not Assigned(Src) or Src.Empty then Exit;
  BeginUpdate;
  try
    DeRect(DstRect, x, y, w, h);
    for I := 0 to w div Src.Width do
      for J := 0 to h div Src.Height do
        Draw(x + I * Src.Width, y + J * Src.Height, Src);
  finally
    EndUpdate;
  end;
end;

// Æ½ÆÌ»æÖÆ×ÔÉíµ½ DC
procedure TCnBitmap.TileDrawTo(hDst: HDC; DstRect: TRect);
var
  I, J, x, y, w, h: Integer;
  Bmp: TCnBitmap;
begin
  if Empty then Exit;
  if Transparent then         // Í¸Ã÷»æÖÆÊ±Ôö¼ÓÒ»¼¶»º³å
  begin
    Bmp := TCnBitmap.Create;
    try
      Bmp.SetSize(DstRect);
      Bmp.Draw(0, 0, hDst, DstRect);
      Bmp.TileDraw(Self);
      Bmp.DrawTo(hDst, DstRect.Left, DstRect.Top);
    finally
      Bmp.Free;
    end;
  end
  else
  begin
    DeRect(DstRect, x, y, w, h);
    for I := 0 to w div Width do
      for J := 0 to h div Height do
        DrawTo(hDst, x + I * Width, y + J * Height);
  end;
end;

//--------------------------------------------------------//
// Í¼ÏñËõ·Å´¦Àí¹ı³Ì                                       //
// Ô­Ê¼Ëã·¨£ºFastLib                                      //
// Ëã·¨ĞŞ¸Ä£ºÖÜ¾¢Óğ£¨Ôö¼ÓÍ¸Ã÷»æÖÆ¹¦ÄÜ¡¢À©Õ¹ºÍ²¿·ÖĞŞÕı£© //
//--------------------------------------------------------//

// ×îÁÚ½ü²åÖµËã·¨Ëõ·Å£¨´Ö²Ú£©
procedure TCnBitmap.NormalResize(Dst: TCnBitmap);
var
  xCount, yCount: Integer;
  x, y, xP, yP, xD, yD: Integer;
  yiScale, xiScale: Integer;
  xScale, yScale: Single;
  Read, Line: PCnLine;
  Tmp: TCnColor;
  pc: PCnColor;
  Tran: Boolean;
  TranColor: TCnColor;
begin
  Tran := Transparent;
  TranColor := GetTranColor;
  xScale := Dst.Width / Width;
  yScale := Dst.Height / Height;
  if (xScale < 1) or (yScale < 1) then // ËõĞ¡
  begin
    xiScale := (Width shl 16) div Dst.Width;
    yiScale := (Height shl 16) div Dst.Height;
    yP := 0;
    if Tran then              // Í¸Ã÷
    begin
      for y := 0 to Dst.Height - 1 do
      begin
        xP := 0;
        Read := FScanLine[yP shr 16]; // Ô´Í¼Ïñ×îÁÚ½üĞĞ
        pc := @Dst.FScanLine[y][0];
        for x := 0 to Dst.Width - 1 do
        begin
          with Read[xP shr 16] do // Í¸Ã÷´¦Àí
            if (b <> TranColor.b) or (g <> TranColor.g) or (r <> TranColor.r) then
              pc^ := Read[xP shr 16]; // Ô´Í¼Ïñ×îÁÚ½üÏñËØ
          Inc(pc);
          Inc(xP, xiScale);
        end;
        Inc(yP, yiScale);
      end;
    end
    else
    begin                     // ²»Í¸Ã÷
      for y := 0 to Dst.Height - 1 do
      begin
        xP := 0;
        Read := FScanLine[yP shr 16]; // Ô´Í¼Ïñ×îÁÚ½üĞĞ
        pc := @Dst.FScanLine[y][0];
        for x := 0 to Dst.Width - 1 do
        begin
          pc^ := Read[xP shr 16]; // Ô´Í¼Ïñ×îÁÚ½üÏñËØ
          Inc(pc);
          Inc(xP, xiScale);
        end;
        Inc(yP, yiScale);
      end;
    end;
  end
  else
  begin                       // ·Å´ó
    if Tran then              // Í¸Ã÷
    begin
      for y := 0 to Height - 1 do
      begin
        yP := Round(yScale * y); // Ä¿±êÍ¼Ïñ×îÁÚ½üÆğÊ¼ĞĞ
        yD := Round(yScale * (y + 1)) - 1; // Ä¿±êÍ¼Ïñ×îÁÚ½ü½áÊøĞĞ
        if yD > Dst.Height - 1 then yD := Dst.Height - 1;
        Read := FScanLine[y]; // Ô´Í¼Ïñµ±Ç°ĞĞ
        for x := 0 to Width - 1 do
        begin
          Tmp := Read[x];     // Ô´Í¼Ïñµ±Ç°ÏñËØ
          if (Tmp.b <> TranColor.b) or (Tmp.g <> TranColor.g) or
            (Tmp.r <> TranColor.r) then // Í¸Ã÷´¦Àí
          begin
            xP := Round(xScale * x); // Ä¿±êÍ¼Ïñ×îÁÚ½üÆğÊ¼ÏñËØ
            xD := Round(xScale * (x + 1)) - 1; // Ä¿±êÍ¼Ïñ×îÁÚ½ü½áÊøÏñËØ
            if xD > Dst.Width - 1 then xD := Dst.Width - 1;
            for xCount := xP to xD do
              Dst.FScanLine[yP][xCount] := Tmp; // ¸´ÖÆÒ»ĞĞ
            for yCount := yP + 1 to yD do
              Move(Dst.FScanLine[yP][xP], Dst.FScanLine[yCount][xP],
                (xD - xP + 1) * 3); //¸´ÖÆµ½ÏàÁÚĞĞ
          end;
        end;
      end;
    end
    else
    begin                     // ²»Í¸Ã÷
      yiScale := Round(yScale + 0.5); // ·Å´ó±¶Êı£¨´óÖµ£©
      xiScale := Round(xScale + 0.5);
      GetMem(Line, Dst.Width * 3); // ÁÙÊ±É¨ÃèÏß
      for y := 0 to Height - 1 do
      begin
        yP := Round(yScale * y); // Ä¿±êÍ¼Ïñ×îÁÚ½üÆğÊ¼ĞĞ
        Read := FScanLine[y];    // Ô´Í¼Ïñµ±Ç°ĞĞ
        for x := 0 to Width - 1 do
        begin
          xP := Round(xScale * x); // Ä¿±êÍ¼Ïñ×îÁÚ½üÆğÊ¼ÏñËØ
          Tmp := Read[x];     // Ô´Í¼Ïñµ±Ç°ÏñËØ
          for xCount := 0 to xiScale - 1 do
          begin
            xD := xCount + xP;
            if xD >= Dst.Width then Break;
            Line[xD] := Tmp;
          end;
        end;
        for yCount := 0 to yiScale - 1 do // ¸´ÖÆµ½ÁÚ½üĞĞ
        begin
          yD := yCount + yP;
          if yD >= Dst.Height then Break;
          CopyMemory(Dst.FScanLine[yD], Line, Dst.Width * 3);
        end;
      end;
      FreeMem(Line);
    end;
  end;
end;

// ¿ìËÙ¶ş´Î²åÖµËã·¨Ëõ·Å£¨Æ½»¬£©
procedure TCnBitmap.SmoothResize(Dst: TCnBitmap);
var
  x, y, xP, yP: Integer;
  yP2, xP2: Integer;
  Read, Read2: PCnLine;
  t, z, z2, iz2: Integer;
  pc: PCnColor;
  w1, w2, w3, w4: Integer;
  Col1, Col2, Col3, Col4: PCnColor;
  Tran: Boolean;
  TranColor: TCnColor;
begin
  Tran := Transparent;
  TranColor := GetTranColor;
  if (Width = 1) or (Height = 1) then
  begin
    NormalResize(Dst);
    Exit;
  end;
  xP2 := ((Width - 1) shl 15) div Dst.Width; // Ëõ·Å±ÈÀı
  yP2 := ((Height - 1) shl 15) div Dst.Height;
  yP := 0;
  for y := 0 to Dst.Height - 1 do
  begin
    pc := @Dst.FScanLine[y][0]; // Ä¿±êÉ¨ÃèÏß
    Read := FScanLine[yP shr 15]; // Ô´ÉÏÉ¨ÃèÏß
    Read2 := FScanLine[yP shr 15 + 1]; // Ô´ÏÂÉ¨ÃèÏß
    z2 := yP and $7FFF;       // Ô´¼ÆËãĞĞÓëÉÏÉ¨ÃèÏßÖ®²î "y"
    iz2 := $8000 - z2;        // Ô´¼ÆËãĞĞÓëÏÂÉ¨ÃèÏßÖ®²î "1-y"
    xP := 0;
    for x := 0 to Dst.Width - 1 do
    begin
      t := xP shr 15;
      z := xP and $7FFF;      // Ô´¼ÆËãÏñËØÓë×óÏñËØÖ®²î "x"
      Col1 := @Read[t];       // ×óÉÏÏñËØ "f(0,0)"
      Col2 := @Read[t + 1];   // ÓÒÉÏÏñËØ "f(1,0)"
      Col3 := @Read2[t];      // ×óÏÂÏñËØ "f(0,1)"
      Col4 := @Read2[t + 1];  // ÓÒÏÂÏñËØ "f(1,1)"
      if Tran then
        with TranColor do
        begin                 // Í¸Ã÷Ê±È¡Ä¿±êÏñËØ
          if (Col1.b = b) and (Col1.g = g) and (Col1.r = r) then Col1 := pc;
          if (Col2.b = b) and (Col2.g = g) and (Col2.r = r) then Col2 := pc;
          if (Col3.b = b) and (Col3.g = g) and (Col3.r = r) then Col3 := pc;
          if (Col4.b = b) and (Col4.g = g) and (Col4.r = r) then Col4 := pc;
        end;                  // ÖÁÉÙÓĞÒ»µã²»Í¸Ã÷
      if (Col1 <> pc) or (Col2 <> pc) or (Col3 <> pc) or (Col4 <> pc) then
      begin
        // ¼ÆËã¼ÓÈ¨Öµ
        w2 := (z * iz2) shr 15; // ÓÒÉÏ p(1,0) = x(1-y);
        w1 := iz2 - w2;         // ×óÉÏ p(0,0) = (1-y)(1-x) = (1-y)-p(0,1)
        w4 := (z * z2) shr 15;  // ÓÒÏÂ p(1,1) = x*y
        w3 := z2 - w4;          // ×óÏÂ p(0,1) = y(1-x) = y-p(1,1)
        // f(x,y) = [f(1,0) - f(0,0)]x + [f(0,1) - f(0,0)y +
        //          [f(1,1) + (f0,0) - f(0,1) - f(1,0)]xy + f(0,0)
        //        = f(0,0)p(0,0) + f(1,0)p(1,0) + f(0,1)p(0,1) + f(1,1)p(1,1)
        pc.b := (Col1.b * w1 + Col2.b * w2 + Col3.b * w3 + Col4.b * w4) shr 15;
        pc.g := (Col1.g * w1 + Col2.g * w2 + Col3.g * w3 + Col4.g * w4) shr 15;
        pc.r := (Col1.r * w1 + Col2.r * w2 + Col3.r * w3 + Col4.r * w4) shr 15;
      end;
      Inc(pc);
      Inc(xP, xP2);
    end;
    Inc(yP, yP2);
  end;
end;

//--------------------------------------------------------//
// Í¼ÏñËõ·Å»æÖÆ¹ı³Ì                                       //
// Ëã·¨Éè¼Æ£ºÖÜ¾¢Óğ                                       //
//--------------------------------------------------------//

// Ëõ·Å»æÖÆ TCnBitmap Î»Í¼
procedure TCnBitmap.StretchDraw(Src: TCnBitmap);
begin
  if Empty or not Assigned(Src) or Src.Empty then Exit;
  BeginUpdate;
  try
    if (Src.Width = Width) and (Src.Height = Height) then
    begin
      Draw(0, 0, Src);        // ³ß´çÏàµÈ
      Exit;
    end;
    if SmoothFilter then      // Æ½»¬Ëõ·Å
      Src.SmoothResize(Self)  // ¶ş´Î²åÖµËã·¨
    else
      Src.NormalResize(Self); // ×îÁÚ½ü²åÖµËã·¨
  finally
    EndUpdate;
  end;
end;

// Ëõ·Å»æÖÆ TCnBitmap Î»Í¼ÔöÇ¿°æ
procedure TCnBitmap.StretchDrawEx(DstRect, SrcRect: TRect; Src: TCnBitmap);
var
  SrcBmp, DstBmp: TCnBitmap;
  x, y, w, h: Integer;
begin
  if Empty or not Assigned(Src) or Src.Empty then Exit;
  BeginUpdate;
  try
    SrcBmp := nil;
    try
      if RectEqu(SrcRect, Src.ClientRect) then
        SrcBmp := Src
      else
      begin
        SrcBmp := TCnBitmap.Create; // Ô´Î»Í¼ÖĞµÄÔ´¾ØĞÎÇøÓò
        SrcBmp.SetSize(SrcRect);
        SrcBmp.Transparent := Src.Transparent;
        SrcBmp.TransparentColor := Src.TransparentColor;
        SrcBmp.DoDraw(0, 0, Src, SrcRect, False); // ²»Í¸Ã÷»æÖÆ
      end;
      if RectEqu(DstRect, ClientRect) then
        StretchDraw(SrcBmp)
      else
      begin
        DstBmp := TCnBitmap.Create; // Ä¿±êÎ»Í¼µÄÄ¿±ê¾ØĞÎÇøÓò
        try
          DeRect(DstRect, x, y, w, h);
          DstBmp.SetSize(w, h);
          if SrcBmp.Transparent then // Í¸Ã÷Ê±¸´ÖÆ×ÔÉí
            DstBmp.DoDraw(0, 0, Self, DstRect, False);
          DstBmp.SmoothFilter := SmoothFilter; // Ëõ·Å·½Ê½
          DstBmp.StretchDraw(SrcBmp);
          Draw(x, y, DstBmp);
        finally
          DstBmp.Free;
        end;
      end;
    finally
      if Assigned(SrcBmp) and (SrcBmp <> Src) then SrcBmp.Free;
    end;
  finally
    EndUpdate;
  end;
end;

// Ëõ·Å»æÖÆ TGraphic
procedure TCnBitmap.StretchDraw(Src: TGraphic);
begin
  StretchDrawEx(ClientRect, Rect(0, 0, Src.Width, Src.Height), Src);
end;

// Ëõ·Å»æÖÆ TGraphic ÔöÇ¿°æ
procedure TCnBitmap.StretchDrawEx(DstRect, SrcRect: TRect; Src: TGraphic);
var
  SrcBmp: TCnBitmap;
begin
  if Empty or not Assigned(Src) or Src.Empty then Exit;
  BeginUpdate;
  try
    SrcBmp := TCnBitmap.Create; // Ô´¾ØĞÎ
    try
      SrcBmp.SetSize(SrcRect);
      SrcBmp.DrawEx(0, 0, Src, SrcRect);
      StretchDrawEx(DstRect, SrcBmp.ClientRect, SrcBmp);
    finally
      SrcBmp.Free;
    end;
  finally
    EndUpdate;
  end;
end;

// Ëõ·Å»æÖÆ DC
procedure TCnBitmap.StretchDraw(SrcRect: TRect; hSrc: HDC);
var
  SrcBmp: TCnBitmap;
begin
  if Empty then Exit;
  BeginUpdate;
  try
    SrcBmp := TCnBitmap.Create; // Ô´¾ØĞÎ
    try
      SrcBmp.SetSize(SrcRect);
      SrcBmp.Draw(0, 0, hSrc, SrcRect);
      StretchDraw(SrcBmp);
    finally
      SrcBmp.Free;
    end;
  finally
    EndUpdate;
  end;
end;

// Ëõ·Å»æÖÆ DC ÔöÇ¿°æ
procedure TCnBitmap.StretchDrawEx(DstRect, SrcRect: TRect; hSrc: HDC);
var
  DstBmp: TCnBitmap;
  x, y, w, h: Integer;
begin
  if Empty then Exit;
  BeginUpdate;
  try
    DstBmp := TCnBitmap.Create; // Ä¿±ê¾ØĞÎ
    try
      DeRect(DstRect, x, y, w, h);
      DstBmp.SetSize(w, h);
      DstBmp.SmoothFilter := SmoothFilter;
      DstBmp.StretchDraw(SrcRect, hSrc);
      Draw(x, y, DstBmp);
    finally
      DstBmp.Free;
    end;
  finally
    EndUpdate;
  end;
end;

// ×ÔÉíËõ·Å»æÖÆµ½ TImage
procedure TCnBitmap.StretchDrawTo(Dst: TImage);
begin
  if Assigned(Dst) then
  begin
    StretchDrawTo(Dst.Canvas.Handle, Dst.ClientRect);
    Dst.Refresh;
  end;
end;

// ×ÔÉíËõ·Å»æÖÆµ½ DC
procedure TCnBitmap.StretchDrawTo(hDst: HDC; DstRect: TRect);
var
  DstBmp: TCnBitmap;
begin
  if Empty then Exit;
  DstBmp := TCnBitmap.Create;
  try
    DstBmp.SetSize(DstRect);
    DstBmp.SmoothFilter := SmoothFilter;
    DstBmp.StretchDraw(Self);
    DstBmp.DrawTo(hDst, DstRect.Left, DstRect.Top);
  finally
    DstBmp.Free;
  end;
end;

// ×ÔÉíËõ·Å»æÖÆµ½ DC ÔöÇ¿°æ
procedure TCnBitmap.StretchDrawToEx(hDst: HDC; DstRect, SrcRect: TRect);
var
  SrcBmp: TCnBitmap;
begin
  if Empty then Exit;
  SrcBmp := TCnBitmap.Create;
  try
    SrcBmp.SetSize(SrcRect);
    SrcBmp.DrawEx(0, 0, Self, SrcRect);
    SrcBmp.SmoothFilter := SmoothFilter;
    SrcBmp.StretchDrawTo(hDst, DstRect);
  finally
    SrcBmp.Free;
  end;
end;

//--------------------------------------------------------//
// Alpha »ìºÏ»æÖÆ                                         //
// Ëã·¨À´Ô´£ºFastLib¡¢pnBitmap                            //
// Ëã·¨¸Ä½ø£ºÖÜ¾¢Óğ£¨Ôö¼ÓÍ¸Ã÷»æÖÆ¹¦ÄÜ¡¢ÔöÇ¿¹¦ÄÜ¼°¸Ä½ø£©   //
//--------------------------------------------------------//

// ¼ì²éÔ´Í¼ÏñÊÇ·ñĞèÒªËõ·Å
function TCnBitmap.CheckAlphaSrc(Src: TCnBitmap; ARect: TRect;
  Stretch: Boolean): TCnBitmap;
begin
  if (RectWidth(ARect) <> Src.Width) or (RectHeight(ARect) <> Src.Height) then
  begin                       // ĞèÒªËõ·Å
    if not Stretch then       // ÓÃÓÚ»ìºÏµÄÎ»Í¼´óĞ¡±ØĞëÏàµÈ
      raise ECnGraphics.Create(SInvalidAlphaBitmap)
    else
    begin                     // ´´½¨ÁÙÊ±Î»Í¼
      Result := TCnBitmap.Create;
      Result.SetSize(ARect);
      Result.SmoothFilter := SmoothFilter;
      if Src.Transparent then // Í¸Ã÷Ê±ÏÈ»æÖÆ×ÔÉí
        Result.DoDraw(0, 0, Self, ARect, False);
      Result.StretchDraw(Src);
    end;
  end
  else
    Result := Src;
end;

// Alpha »ìºÏ»æÖÆ
procedure TCnBitmap.AlphaDraw(Src: TCnBitmap; Alpha: TCnAlpha;
  Stretch: Boolean);
var
  x, y, I: Integer;
  c1, c2: PCnColor;
  Table: array[-255..255] of Integer;
  Bmp: TCnBitmap;
  TranColor: TCnColor;
  FAlpha: Integer;
begin
  if Empty or not Assigned(Src) or Src.Empty then Exit;
  FAlpha := AlphaToInt(Alpha);
  if FAlpha = 0 then Exit;
  Bmp := nil;
  BeginUpdate;
  try
    Bmp := CheckAlphaSrc(Src, ClientRect, Stretch); // ´¦ÀíÔ´Î»Í¼Ëõ·Å
    if FAlpha = 255 then
    begin
      Draw(0, 0, Bmp);
      Exit;
    end;

    for I := -255 to 255 do   // ½¨Á¢Alpha»ìºÏ±í
      Table[I] := (FAlpha * I) shr 8;

    c1 := Bits;
    c2 := Bmp.Bits;
    if (Bmp = Src) and Src.Transparent then  // Ô´Î»Í¼Í¸Ã÷ÇÒÎ´Ëõ·Å£¨Ëõ·ÅÊ±ÒÑ´¦Àí¹ıÍ¸Ã÷ÁË£©
    begin
      TranColor := Src.GetTranColor; // »ìºÏÊ±Í¸Ã÷´¦Àí
      for y := 0 to FHeight - 1 do
      begin
        for x := 0 to FWidth - 1 do
        begin
          if (TranColor.b <> c2.b) or (TranColor.g <> c2.g) or (TranColor.r <> c2.r)
            then
          begin
            c1.b := Table[c2.b - c1.b] + c1.b; // ²é±í·¨¿ìËÙ»ìºÏ
            c1.g := Table[c2.g - c1.g] + c1.g;
            c1.r := Table[c2.r - c1.r] + c1.r;
          end;
          Inc(c1);
          Inc(c2);
        end;
        c1 := Pointer(TCnNativeInt(c1) + Gap);
        c2 := Pointer(TCnNativeInt(c2) + Bmp.Gap);
      end;
    end
    else
    begin
      for y := 0 to FHeight - 1 do
      begin
        for x := 0 to FWidth - 1 do
        begin
          c1.b := Table[c2.b - c1.b] + c1.b; // ²é±í·¨¿ìËÙ»ìºÏ
          c1.g := Table[c2.g - c1.g] + c1.g;
          c1.r := Table[c2.r - c1.r] + c1.r;
          Inc(c1);
          Inc(c2);
        end;
        c1 := Pointer(TCnNativeInt(c1) + Gap);
        c2 := Pointer(TCnNativeInt(c2) + Bmp.Gap);
      end;
    end;
  finally
    if Assigned(Bmp) and (Bmp <> Src) then Bmp.Free; // ÊÍ·ÅÁÙÊ±Î»Í¼
    EndUpdate;
  end;
end;

// Alpha »ìºÏ»æÖÆ£¨ÔÊĞíÖ¸¶¨Î»ÖÃ£©
// Ëã·¨Éè¼Æ£ºÖÜ¾¢Óğ
procedure TCnBitmap.AlphaDraw(DstX, DstY: Integer; Src: TCnBitmap;
  SrcRect: TRect; Alpha: TCnAlpha);
var
  I, J: Integer;
  p1, p2: PCnColor;
  x, y, sx, sy, w, h: Integer;
  Tran: Boolean;
  TranColor: TCnColor;
  Table: array[-255..255] of Integer;
  FAlpha: Integer;
begin
  if Empty or not Assigned(Src) or Src.Empty or not CalcDrawRect(DstX, DstY,
    SrcRect, Src.ClientRect, x, y, sx, sy, w, h) then Exit;

  FAlpha := AlphaToInt(Alpha);
  if FAlpha = 0 then Exit;
  if FAlpha = 255 then
  begin
    DrawEx(DstX, DstY, Src, SrcRect);
    Exit;
  end;

  Changing;
  for I := -255 to 255 do     // ½¨Á¢ Alpha »ìºÏ±í
    Table[I] := (FAlpha * I) shr 8;
  Tran := Src.Transparent;
  TranColor := Src.GetTranColor;
  for I := 0 to h - 1 do
  begin
    p1 := @FScanLine[y + I][x];
    P2 := @Src.FScanLine[sy + I][sx];
    for J := 0 to w - 1 do
    begin                     // Í¸Ã÷ÅĞ¶Ï
      if not Tran or (p2.b <> TranColor.b) or (p2.g <> TranColor.g) or (p2.r <>
        TranColor.r) then
      begin
        p1.b := Table[p2.b - p1.b] + p1.b; // ²é±í·¨¿ìËÙ»ìºÏ
        p1.g := Table[p2.g - p1.g] + p1.g;
        p1.r := Table[p2.r - p1.r] + p1.r;
      end;
      Inc(p1);
      Inc(p2);
    end;
  end;
  Changed;
end;

// ½¥±äÍ¸Ã÷µÄ Alpha »ìºÏ²Ù×÷
// Ëã·¨Éè¼Æ£ºÖÜ¾¢Óğ
procedure TCnBitmap.AlphaDrawGrad(Src: TCnBitmap; Style: TCnGradStyle;
  Stretch: Boolean; StartAlpha: TCnAlpha; EndAlpha: TCnAlpha);
var
  x, y, I: Integer;
  c1, c2: PCnColor;
  Bmp: TCnBitmap;
  Tran: Boolean;
  TranColor: TCnColor;
  SA, EA, AddA, CurA: Integer;
  BufLen, Len: Integer;
  Alpha: PByteArray;
  ox, oy, Rate: Double;
  ta, tb, tab: Double;
  Weight: Integer;
begin
  if Empty or not Assigned(Src) or Src.Empty then Exit;

  Bmp := nil;
  Alpha := nil;
  BeginUpdate;
  try
    Bmp := CheckAlphaSrc(Src, ClientRect, Stretch); // ´¦ÀíÔ´Î»Í¼Ëõ·Å
    Tran := (Bmp = Src) and Src.Transparent; // Ô´Î»Í¼Í¸Ã÷ÇÒÎ´Ëõ·Å
    TranColor := Src.GetTranColor;
    // ¼ÆËã½¥±ä Alpha ±í
    if Style in [gsLeftToRight, gsRightToLeft, gsCenterToLR] then
      BufLen := FWidth        // »º³åÇø³¤¶È
    else if Style in [gsTopToBottom, gsBottomToTop, gsCenterToTB] then
      BufLen := FHeight
    else if Style = gsRadial then
      BufLen := Max(FWidth, FHeight)
    else
      Exit;
    if Style in [gsCenterToLR, gsCenterToTB] then
      Len := (BufLen + 1) div 2 // ½¥±ä´ø³¤¶È
    else
      Len := BufLen;

    GetMem(Alpha, BufLen);
    if Style in [gsLeftToRight, gsTopToBottom, gsRadial] then
    begin
      SA := AlphaToInt(StartAlpha) shl 16; // ÕıÏò½¥±ä
      EA := AlphaToInt(EndAlpha) shl 16;
    end else
    begin
      SA := AlphaToInt(EndAlpha) shl 16;
      EA := AlphaToInt(StartAlpha) shl 16;
    end;
    AddA := Round((EA - SA) / Len); // Ã¿ÏñËØÔöÁ¿
    CurA := SA;
    for I := 0 to Len - 1 do
    begin
      Alpha[I] := CurA shr 16; // Ğ¡Êı×ªÕûÊıÓÅ»¯
      Inc(CurA, AddA);
    end;

    if Style in [gsCenterToLR, gsCenterToTB] then // ¶Ô³Æ½¥±ä
      for I := 0 to Len - 1 do
        Alpha[BufLen - 1 - I] := Alpha[I];

    c1 := Bits;
    c2 := Bmp.Bits;
    if Style in [gsLeftToRight, gsRightToLeft, gsCenterToLR] then
    begin                     // Ë®Æ½·½Ïò½¥±ä
      for y := 0 to FHeight - 1 do
      begin
        for x := 0 to FWidth - 1 do
        begin
          if not Tran or (TranColor.b <> c2.b) or (TranColor.g <> c2.g) or
            (TranColor.r <> c2.r) then
          begin
            c1.b := Alpha[x] * (c2.b - c1.b) shr 8 + c1.b;
            c1.g := Alpha[x] * (c2.g - c1.g) shr 8 + c1.g;
            c1.r := Alpha[x] * (c2.r - c1.r) shr 8 + c1.r;
          end;
          Inc(c1);
          Inc(c2);
        end;
        c1 := Pointer(TCnNativeInt(c1) + Gap);
        c2 := Pointer(TCnNativeInt(c2) + Bmp.Gap);
      end;
    end else if Style in [gsTopToBottom, gsBottomToTop, gsCenterToTB] then
    begin                     // ´¹Ö±·½Ïò½¥±ä
      for y := 0 to FHeight - 1 do
      begin
        for x := 0 to FWidth - 1 do
        begin
          if not Tran or (TranColor.b <> c2.b) or (TranColor.g <> c2.g) or
            (TranColor.r <> c2.r) then
          begin
            c1.b := Alpha[y] * (c2.b - c1.b) shr 8 + c1.b;
            c1.g := Alpha[y] * (c2.g - c1.g) shr 8 + c1.g;
            c1.r := Alpha[y] * (c2.r - c1.r) shr 8 + c1.r;
          end;
          Inc(c1);
          Inc(c2);
        end;
        c1 := Pointer(TCnNativeInt(c1) + Gap);
        c2 := Pointer(TCnNativeInt(c2) + Bmp.Gap);
      end;
    end
    else if Style = gsRadial then
    begin                     // ·øÉä½¥±ä
      ta := FWidth / 2;       // ÍÖÔ²³¤Öá
      tb := FHeight / 2;      // ÍÖÔ²¶ÌÖá
      tab := ta * tb;
      for y := 0 to FHeight - 1 do
      begin
        oy := Abs(y - tb);    // ´¹Ö±ÖĞĞÄ¾à
        for x := 0 to FWidth - 1 do
        begin
          ox := Abs(x - ta);  // Ë®Æ½ÖĞĞÄ¾à
          if ox = 0 then
            Rate := oy / tb
          else if oy = 0 then
            Rate := ox / ta
          else                // ¼ÆËãµ±Ç°µãÖĞĞÄ¾àÓë¸Ã·½Ïò°ë¾¶Ö®±È
            Rate := ox * Hypot(tb, ta * oy / ox) / tab;
          if Rate >= 1 then
            Weight := Alpha[BufLen - 1]
          else                // µ±Ç°µã½¥±äAlphaÖµ
            Weight := Alpha[Round(Rate * (BufLen - 1))];
          if not Tran or (TranColor.b <> c2.b) or (TranColor.g <> c2.g) or
            (TranColor.r <> c2.r) then
          begin
            c1.b := Weight * (c2.b - c1.b) shr 8 + c1.b;
            c1.g := Weight * (c2.g - c1.g) shr 8 + c1.g;
            c1.r := Weight * (c2.r - c1.r) shr 8 + c1.r;
          end;
          Inc(c1);
          Inc(c2);
        end;
        c1 := Pointer(TCnNativeInt(c1) + Gap);
        c2 := Pointer(TCnNativeInt(c2) + Bmp.Gap);
      end
    end;
  finally
    if Assigned(Bmp) and (Bmp <> Src) then Bmp.Free; // ÊÍ·ÅÁÙÊ±Î»Í¼
    if Alpha <> nil then FreeMem(Alpha);
    EndUpdate;
  end;
end;

// ÔöÇ¿µÄ Alpha »ìºÏ²Ù×÷£¨Ç°¾°¡¢±³¾°»ìºÏµ½×ÔÉí£©
// Ëã·¨Éè¼Æ£ºÖÜ¾¢Óğ
procedure TCnBitmap.AlphaDrawEx(DstRect: TRect; Front, Back: TCnBitmap;
  Alpha: TCnAlpha; Stretch: Boolean);
var
  x, y, I: Integer;
  c1, c2, c3: PCnColor;
  cFt, cBk: PCnColor;
  Table: array[-255..255] of Integer;
  BmpFt, BmpBk: TCnBitmap;
  xd, yd, xs, ys, w, h: Integer;
  TranFt, TranBk: Boolean;
  TranColorFt, TranColorBk: TCnColor;
  FAlpha: Integer;
begin
  if Empty or not Assigned(Front) or Front.Empty or not Assigned(Back) or
    Back.Empty then Exit;
  BmpFt := nil;
  BmpBk := nil;
  BeginUpdate;
  try
    DeRect(DstRect, xd, yd, w, h);
    if (xd > Width - 1) or (yd > Height - 1) then Exit;
    xs := 0;
    ys := 0;
    if xd < 0 then            // Ä¿±ê×ø±ê³¬³ö·¶Î§
    begin
      Inc(w, xd);
      Dec(xs, xd);
      xd := 0;
    end;
    if yd < 0 then
    begin
      Inc(h, yd);
      Dec(ys, yd);
      yd := 0;
    end;
    if xd + w > Width then Dec(w, xd + w - Width);
    if yd + h > Height then Dec(h, yd + h - Height);

    BmpFt := CheckAlphaSrc(Front, DstRect, Stretch); // ´¦ÀíÔ´Î»Í¼Ëõ·Å
    BmpBk := CheckAlphaSrc(Back, DstRect, Stretch);
    TranFt := (BmpFt = Front) and Front.Transparent;
    TranColorFt := Front.GetTranColor;
    TranBk := (BmpBk = Back) and Back.Transparent;
    TranColorBk := Back.GetTranColor;

    FAlpha := AlphaToInt(Alpha);
    for I := -255 to 255 do   // ½¨Á¢ Alpha »ìºÏ±í
      Table[I] := (FAlpha * I) shr 8;

    if TranFt or TranBk then  // ĞèÒª´¦ÀíÍ¸Ã÷£¨ËÙ¶ÈÏÂ½µÔ¼ 20%£©
    begin
      for y := 0 to h - 1 do
      begin
        c1 := @FScanLine[y + yd][xd];
        c2 := @BmpFt.FScanLine[y + ys][xs];
        c3 := @BmpBk.FScanLine[y + ys][xs];
        for x := 0 to w - 1 do
        begin
          if not TranFt or (TranColorFt.b <> c2.b) or (TranColorFt.g <> c2.g)
            or (TranColorFt.r <> c2.r) then // Ç°¾°Í¼Í¸Ã÷ÅĞ¶Ï
            cFt := c2
          else
            cFt := c1;
          if not TranBk or (TranColorBk.b <> c2.b) or (TranColorBk.g <> c2.g)
            or (TranColorBk.r <> c2.r) then // ±³¾°Í¼Í¸Ã÷ÅĞ¶Ï
            cBk := c3
          else
            cBk := c1;
          c1.b := Table[cFt.b - cBk.b] + cBk.b; // ²é±í·¨¿ìËÙ»ìºÏ
          c1.g := Table[cFt.g - cBk.g] + cBk.g;
          c1.r := Table[cFt.r - cBk.r] + cBk.r;
          Inc(c1);
          Inc(c2);
          Inc(c3);
        end;
      end;
    end
    else                      // ²»ĞèÒª´¦ÀíÍ¸Ã÷
    begin
      for y := 0 to h - 1 do
      begin
        c1 := @FScanLine[y + yd][xd];
        c2 := @BmpFt.FScanLine[y + ys][xs];
        c3 := @BmpBk.FScanLine[y + ys][xs];
        for x := 0 to w - 1 do
        begin
          c1.b := Table[c2.b - c3.b] + c3.b; // ²é±í·¨¿ìËÙ»ìºÏ
          c1.g := Table[c2.g - c3.g] + c3.g;
          c1.r := Table[c2.r - c3.r] + c3.r;
          Inc(c1);
          Inc(c2);
          Inc(c3);
        end;
      end;
    end;
  finally
    if Assigned(BmpFt) and (BmpFt <> Front) then BmpFt.Free; // ÊÍ·ÅÁÙÊ±Î»Í¼
    if Assigned(BmpBk) and (BmpBk <> Back) then BmpBk.Free;
    EndUpdate;
  end;
end;

//--------------------------------------------------------//
// ½¥±äÑÕÉ«»æÖÆ                                           //
// Ëã·¨Éè¼Æ£ºÖÜ¾¢Óğ                                       //
//--------------------------------------------------------//

// »æÖÆ½¥±äÉ«
procedure TCnBitmap.DrawGradient(GradColor: TCnGradientColor);
begin
  DrawGradientEx(GradColor, ClientRect, csMaxAlpha);
end;

// »æÖÆ½¥±äÉ«ÔöÇ¿°æ
procedure TCnBitmap.DrawGradientEx(GradColor: TCnGradientColor; Rect: TRect;
  Alpha: TCnAlpha);
var
  Buf: PCnLine;
  BufLen, Len: Integer;
  IsInvert: Boolean;
  SCol, ECol: TCnColor;
  Col, Col1: PCnColor;
  Bd: TCnColor;
  BdWeight: Integer;
  BufSize: Integer;
  x, y, I, J, Cur, SPos, EPos, Added, Head: Integer;
  ARect: TRect;
  Table: array[-255..255] of Integer;
  FAlpha: Integer;
  OffX, OffY, LineSize: Integer;
  cx, cy, ox, oy, lx, Rate: Double;
  ta, tb, ta2, tab, tab2: Double;
  XL, XR: Integer;
begin
  if Empty or not Assigned(GradColor) then Exit;
  FAlpha := AlphaToInt(Alpha);
  if FAlpha = 0 then Exit;
  if not IntersectRect(ARect, ClientRect, Rect) then Exit;

  BeginUpdate;
  try
    if FAlpha < 255 then
      for I := -255 to 255 do // ½¨Á¢ Alpha »ìºÏ±í
        Table[I] := (FAlpha * I) shr 8;

    if GradColor.FStyle in [gsLeftToRight, gsRightToLeft, gsCenterToLR] then
      BufLen := RectWidth(Rect) // »º³åÇø³¤¶È
    else if GradColor.FStyle in [gsTopToBottom, gsBottomToTop, gsCenterToTB] then
      BufLen := RectHeight(Rect)
    else if GradColor.FStyle = gsRadial then
      BufLen := Max(RectWidth(Rect), RectHeight(Rect))
    else
      Exit;
    if GradColor.FStyle in [gsCenterToLR, gsCenterToTB] then
      Len := (BufLen + 1) div 2 // ½¥±ä´ø³¤¶È
    else
      Len := BufLen;
    BufSize := BufLen * 3;
    GetMem(Buf, BufSize);
    try
      // ´´½¨½¥±äÉ«´ø»º³åÇø
      IsInvert := GradColor.FStyle in [gsRightToLeft, gsBottomToTop,
        gsCenterToLR, gsCenterToTB]; // ·´Ğò
      I := 0;
      SCol := CnColor(GradColor.FColorStart); // ÆğÊ¼É«
      SPos := 0;              // ÆğÊ¼Î»ÖÃ
      repeat
        if Assigned(GradColor.FColorMiddle) and (I < GradColor.FColorMiddle.Count) then
        begin                 // ´ÓÖĞ¼äÉ«ÖĞ²éÕÒ
          ECol := CnColor(GradColor.FColorMiddle[I].FColor);
          EPos := GradColor.FColorMiddle[I].FPos;
          Inc(I);
        end
        else
        begin
          ECol := CnColor(GradColor.FColorEnd); // ½áÊøÉ«
          EPos := csMaxGradPos;
        end;
        Head := SPos * Len div csMaxGradPos; // ¿ªÊ¼Î»ÖÃ¼°ÔöÁ¿
        Added := Min((EPos - SPos) * Len div csMaxGradPos + 1, Len - Head);
        for J := 0 to Added - 1 do
        begin
          if IsInvert then    // ¼ì²é·´Ğò
            Cur := Len - 1 - (J + Head)
          else
            Cur := J + Head;
          Buf[Cur].r := SCol.r + (ECol.r - SCol.r) * J div Added;
          Buf[Cur].g := SCol.g + (ECol.g - SCol.g) * J div Added;
          Buf[Cur].b := SCol.b + (ECol.b - SCol.b) * J div Added;
        end;
        SCol := ECol;
        SPos := EPos;
      until EPos = csMaxGradPos;

      if GradColor.FStyle in [gsCenterToLR, gsCenterToTB] then // ¶Ô³Æ½¥±ä
        for I := 0 to Len - 1 do
          Buf[BufLen - 1 - I] := Buf[I];

      OffX := ARect.Left - Rect.Left;
      OffY := ARect.Top - Rect.Top;
      if GradColor.FStyle in [gsLeftToRight, gsRightToLeft, gsCenterToLR] then
      begin                   // Ë®Æ½½¥±ä
        if FAlpha = 255 then  // ²»Í¸Ã÷
        begin
          LineSize := RectWidth(ARect) * 3;
          for I := ARect.Top to ARect.Bottom - 1 do
            Move(Buf[OffX], FScanLine[I][ARect.Left], LineSize);
        end
        else
        begin                 // Í¸Ã÷¶È
          for y := ARect.Top to ARect.Bottom - 1 do
          begin
            Col := @FScanLine[y][ARect.Left];
            Col1 := @Buf[OffX];
            for x := 0 to RectWidth(ARect) - 1 do
            begin             // ²é±í·¨¿ìËÙ»ìºÏ
              Col.b := Table[Col1.b - Col.b] + Col.b;
              Col.g := Table[Col1.g - Col.g] + Col.g;
              Col.r := Table[Col1.r - Col.r] + Col.r;
              Inc(Col);
              Inc(Col1);
            end;
          end;
        end;
      end
      else if GradColor.FStyle in [gsTopToBottom, gsBottomToTop, gsCenterToTB] then
      begin                   // ´¹Ö±½¥±ä
        if FAlpha = 255 then  // ²»Í¸Ã÷
        begin
          for y := 0 to RectHeight(ARect) - 1 do
          begin
            Col := @FScanLine[y + ARect.Top][ARect.Left];
            for x := 0 to RectWidth(ARect) - 1 do
            begin
              Col^ := Buf[y + OffY];
              Inc(Col);
            end;
          end;
        end
        else
        begin                 // Í¸Ã÷¶È
          for y := 0 to RectHeight(ARect) - 1 do
          begin               // ²é±í·¨¿ìËÙ»ìºÏ
            Col := @FScanLine[y + ARect.Top][ARect.Left];
            Col1 := @Buf[y + OffY];
            for x := 0 to RectWidth(ARect) - 1 do
            begin
              Col.b := Table[Col1.b - Col.b] + Col.b;
              Col.g := Table[Col1.g - Col.g] + Col.g;
              Col.r := Table[Col1.r - Col.r] + Col.r;
              Inc(Col);
            end;
          end;
        end;
      end
      else if GradColor.FStyle = gsRadial then
      begin                   // ·øÉä½¥±ä
        ta := RectWidth(Rect) / 2; // ÍÖÔ²³¤Öá
        tb := RectHeight(Rect) / 2; // ÍÖÔ²¶ÌÖá
        ta2 := Sqr(ta);
        tab := ta * tb;
        tab2 := Sqr(tab);
        cx := (Rect.Left + Rect.Right - 1) / 2; // ÖĞĞÄµã
        cy := (Rect.Top + Rect.Bottom - 1) / 2;
        for y := ARect.Top to ARect.Bottom - 1 do
        begin
          oy := Abs(y - cy);  // ´¹Ö±ÖĞĞÄ¾à
          if FSmoothFilter then
            lx := Sqrt(tab2 - ta2 * Sqr(oy - 1)) / tb // ¿¼ÂÇ¿¹¾â³İ´¦Àí
          else                // ¼ÆËãÓĞĞ§»æÖÆ¿í¶È
            lx := Sqrt(tab2 - ta2 * Sqr(oy)) / tb;
          XL := Max(Floor(cx - lx), ARect.Left); // ÍÖÔ²×ó±ß½ç
          XR := Min(Ceil(cx + lx), ARect.Right - 1); // ÓÒ±ß½ç
          Col := @FScanLine[y][XL];
          for x := XL to XR do
          begin
            ox := Abs(x - cx); // Ë®Æ½ÖĞĞÄ¾à
            if ox = 0 then
              Rate := oy / tb
            else if oy = 0 then
              Rate := ox / ta
            else              // ¼ÆËãµ±Ç°µãÖĞĞÄ¾àÓë¸Ã·½Ïò°ë¾¶Ö®±È
              Rate := ox * Hypot(tb, ta * oy / ox) / tab;
            if Rate <= 1 then
            begin             // µ±Ç°µã½¥±äÑÕÉ«Öµ
              Col1 := @Buf[Round(Rate * (BufLen - 1))];
              if FAlpha = 255 then // ²»Í¸Ã÷
                Col^ := Col1^
              else
              begin           // ²é±í·¨¿ìËÙ»ìºÏ
                Col.b := Table[Col1.b - Col.b] + Col.b;
                Col.g := Table[Col1.g - Col.g] + Col.g;
                Col.r := Table[Col1.r - Col.r] + Col.r;
              end;
            end
            else if FSmoothFilter then // ±ß½çµã¿¹¾â³İ´¦Àí
            begin
              BdWeight := Round((1 - (Rate - 1) * (BufLen - 1)) * FAlpha);
              if BdWeight > 0 then // ²»Í¸Ã÷¶È
              begin
                Col1 := @Buf[BufLen - 1];
                Col.b := BdWeight * (Col1.b - Col.b) shr 8 + Col.b;
                Col.g := BdWeight * (Col1.g - Col.g) shr 8 + Col.g;
                Col.r := BdWeight * (Col1.r - Col.r) shr 8 + Col.r;
              end;
            end;
            Inc(Col);
          end;
        end
      end;
    finally
      FreeMem(Buf);
    end;
  finally
    EndUpdate;
  end;
end;

//--------------------------------------------------------//
// °´Å¥Î»Í¼»æÖÆ¹ı³Ì                                       //
// Ëã·¨Éè¼Æ£ºÖÜ¾¢Óğ                                       //
//           ±ÈRxlibÖĞËã·¨¿ìÒ»±¶ÒÔÉÏ                      //
// Ëã·¨²Î¿¼£ºRxlib¡¢pnBitmap                              //
//--------------------------------------------------------//

// Ê§Ğ§Î»Í¼
procedure TCnBitmap.Disabled;
begin
  DisabledEx(CnWinColor(GetTranColor), clBtnface, clBtnHighlight, clBtnShadow, True);
end;

// Ê§Ğ§Î»Í¼ÔöÇ¿°æ
procedure TCnBitmap.DisabledEx(OutlineColor, BackColor, HighlightColor,
  ShadowColor: TColor; DrawHighlight: Boolean);
var
  BmpLight: TCnBitmap;
begin
  if Empty then Exit;
  BeginUpdate;
  try
    MaskEx(OutlineColor, BackColor, ShadowColor); // ¶Ô×ÔÉí¶şÉ«»¯
    TransparentColor := BackColor; // Í¸Ã÷É«¸ÄÎª±³¾°É«
    if DrawHighlight then     // ÔÚÓÒÏÂ½Ç»æÖÆ¸ßÁÁÍ¼
    begin
      BmpLight := TCnBitmap.Create;
      try
        BmpLight.SetSize(ClientRect);
        BmpLight.Fill(BackColor);
        BmpLight.DoDraw(1, 1, Self, ClientRect, True);
        BmpLight.MaskEx(ShadowColor, HighlightColor, BackColor);
        BmpLight.DoDraw(0, 0, Self, ClientRect, True);
        Move(BmpLight.FBits^, FBits^, FSize);
      finally
        BmpLight.Free;
      end;
    end
  finally
    EndUpdate;
  end;
end;

// »æÖÆÊ§Ğ§Î»Í¼
procedure TCnBitmap.DrawDisabled(hDst: HDC; ARect: TRect);
begin
  DrawDisabledEx(hDst, ARect, CnWinColor(GetTranColor), clBtnface,
    clBtnHighlight, clBtnShadow, True);
end;

// »æÖÆÊ§Ğ§Î»Í¼ÔöÇ¿°æ
procedure TCnBitmap.DrawDisabledEx(hDst: HDC; ARect: TRect; OutlineColor, BackColor,
  HighlightColor, ShadowColor: TColor; DrawHighlight: Boolean);
var
  Bmp: TCnBitmap;
begin
  if Empty then Exit;
  Bmp := TCnBitmap.Create;
  try
    Bmp.Assign(Self);
    Bmp.DisabledEx(OutlineColor, BackColor, HighlightColor, ShadowColor,
      DrawHighlight);
    Bmp.DrawTo(hDst, ARect.Left, ARect.Top);
  finally
    Bmp.Free;
  end;
end;

// ÒõÓ°Î»Í¼
procedure TCnBitmap.Shadowed;
begin
  ShadowedEx(CnWinColor(GetTranColor), clBlack, clBtnface, True, 2, 2);
end;

// ÒõÓ°Î»Í¼ÔöÇ¿°æ
procedure TCnBitmap.ShadowedEx(OutlineColor, ShadowColor,
  BackColor: TColor; Blur: Boolean; OffsetX, OffsetY: Integer);
var
  Bmp: TCnBitmap;
begin
  if Empty then Exit;
  BeginUpdate;
  try
    Bmp := TCnBitmap.Create;
    try
      Bmp.SetSize(ClientRect); // ÁÙÊ±Î»Í¼
      Bmp.Fill(BackColor);
      TransparentColor := OutlineColor; // Í¸Ã÷»æÖÆµ½ÁÙÊ±Î»Í¼
      Bmp.DoDraw(-OffsetX, -OffsetY, Self, ClientRect, True);
      Bmp.Transparent := True;
      Bmp.TransparentColor := BackColor; // ¶Ô×ÔÉí¶şÉ«»¯
      MaskEx(OutlineColor, BackColor, ShadowColor);
      TransparentColor := BackColor;
      if Blur then Self.Blur; // ÒõÓ°Ä£ºı
      DoDraw(0, 0, Bmp, ClientRect, True); // ´ÓÁÙÊ±Î»Í¼¸´ÖÆ
    finally
      Bmp.Free;
    end;
  finally
    EndUpdate;
  end;
end;

// »æÖÆÒõÓ°Î»Í¼
procedure TCnBitmap.DrawShadowed(hDst: HDC; ARect: TRect);
begin
  DrawShadowedEx(hDst, ARect, CnWinColor(GetTranColor), clBlack, clBtnface,
    True, 2, 2);
end;

// »æÖÆÒõÓ°Î»Í¼ÔöÇ¿°æ
procedure TCnBitmap.DrawShadowedEx(hDst: HDC; ARect: TRect; OutlineColor,
  ShadowColor, BackColor: TColor; Blur: Boolean; OffsetX, OffsetY: Integer);
var
  Bmp: TCnBitmap;
begin
  if Empty then Exit;
  Bmp := TCnBitmap.Create;
  try
    Bmp.Assign(Self);
    Bmp.ShadowedEx(OutlineColor, ShadowColor, BackColor, Blur, OffsetX, OffsetY);
    Bmp.DrawTo(hDst, ARect.Left, ARect.Top);
  finally
    Bmp.Free;
  end;
end;

//--------------------------------------------------------//
// Í¼ÏñÑÕÉ«ÊôĞÔµ÷Õû                                       //
// Ëã·¨À´Ô´£ºFastLib¡¢pnBitmap                            //
// Ëã·¨ĞŞ¸Ä£ºÖÜ¾¢Óğ                                       //
//--------------------------------------------------------//

// µ÷Õû¸÷ÑÕÉ«·ÖÁ¿
procedure TCnBitmap.RGB(ra, ga, ba: TAdjustRange);
var
  Table: array[0..255] of TCnColor;
  x, y, I: Integer;
  CurBits: PCnColor;
  r, g, b: Integer;
begin
  if Empty then Exit;
  Changing;
  b := RangeToInt(ba, -255, 255);
  g := RangeToInt(ga, -255, 255);
  r := RangeToInt(ra, -255, 255);
  for I := 0 to 255 do
  begin
    Table[I].b := IntToByte(I + b);
    Table[I].g := IntToByte(I + g);
    Table[I].r := IntToByte(I + r);
  end;
  CurBits := Bits;
  for y := 0 to Height - 1 do
  begin
    for x := 0 to Width - 1 do
    begin
      CurBits.b := Table[CurBits.b].b;
      CurBits.g := Table[CurBits.g].g;
      CurBits.r := Table[CurBits.r].r;
      Inc(CurBits);
    end;
    CurBits := Pointer(TCnNativeInt(CurBits) + Gap);
  end;
  Changed;
end;

// µ÷ÕûÍ¼ÏñÁÁ¶È
procedure TCnBitmap.Brightness(Range: TAdjustRange; Channels: TColorChannels);
var
  x, y: Integer;
  Table: array[0..255] of Byte;
  CurBits: PCnColor;
  Amount: Integer;
begin
  if Empty or (Channels = []) then Exit;
  Changing;
  Amount := RangeToInt(Range, -256, 256);
  if Amount > 0 then          // ±äÁÁ
    for x := 0 to 255 do
      Table[x] := IntToByte(x + ((Amount * (x xor 255)) shr 8))
  else
  begin
    Amount := -Amount;        // ±ä°µ
    for x := 0 to 255 do
      Table[x] := IntToByte(x - ((Amount * x) shr 8));
  end;

  CurBits := Bits;
  if Channels = csAllChannels then
  begin
    for y := 1 to FHeight do
    begin
      for x := 1 to FWidth do
      begin
        CurBits.b := Table[CurBits.b];
        CurBits.g := Table[CurBits.g];
        CurBits.r := Table[CurBits.r];
        Inc(CurBits);
      end;
      CurBits := Pointer(TCnNativeInt(CurBits) + Gap);
    end;
  end
  else
  begin
    for y := 1 to FHeight do
    begin
      for x := 1 to FWidth do
      begin
        if ccBlue in Channels then CurBits.b := Table[CurBits.b];
        if ccGreen in Channels then CurBits.g := Table[CurBits.g];
        if ccRed in Channels then CurBits.r := Table[CurBits.r];
        Inc(CurBits);
      end;
      CurBits := Pointer(TCnNativeInt(CurBits) + Gap);
    end;
  end;
  Changed;
end;

// µ÷ÕûÍ¼Ïñ±¥ºÍ¶È
procedure TCnBitmap.Saturation(Range: TAdjustRange; Channels: TColorChannels);
var
  Grays: array[0..255] of Byte;
  Alpha: array[0..255] of WORD;
  Gray: Byte;
  x, y, ag: Integer;
  CurBits: TCnColor;
  pc: PCnColor;
  Amount: Integer;
begin
  if Empty or (Channels = []) then Exit;
  Changing;
  x := 0;
  y := 0;
  for ag := 0 to 85 do
  begin
    Grays[x + 0] := y;        // Grays[i] := i div 3
    Grays[x + 1] := y;
    Grays[x + 2] := y;
    Inc(y);
    Inc(x, 3);
  end;

  Amount := RangeToInt(Range, 0, 510);
  for x := 0 to 255 do
    Alpha[x] := (x * Amount) shr 8;
  pc := Bits;
  if Channels = csAllChannels then
  begin
    for y := 0 to FHeight - 1 do
    begin
      for x := 0 to FWidth - 1 do
      begin                   // ²é±í·¨
        CurBits := pc^;       // Gray := (r + g + b) div 3
        Gray := Grays[CurBits.r] + Grays[CurBits.g] + Grays[CurBits.b];
        ag := Gray - Alpha[Gray]; // r := r * Alpha + Gray * (1 - Alpha)
        pc.b := IntToByte(Alpha[CurBits.b] + ag);
        pc.g := IntToByte(Alpha[CurBits.g] + ag);
        pc.r := IntToByte(Alpha[CurBits.r] + ag);
        Inc(pc);
      end;
      pc := Pointer(TCnNativeInt(pc) + Gap);
    end;
  end
  else
  begin
    for y := 0 to FHeight - 1 do
    begin
      for x := 0 to FWidth - 1 do
      begin                   // ²é±í·¨
        CurBits := pc^;       // Gray := (r + g + b) div 3
        Gray := Grays[CurBits.r] + Grays[CurBits.g] + Grays[CurBits.b];
        ag := Gray - Alpha[Gray]; // r := r * Alpha + Gray * (1 - Alpha)
        if ccBlue in Channels then pc.b := IntToByte(Alpha[CurBits.b] + ag);
        if ccGreen in Channels then pc.g := IntToByte(Alpha[CurBits.g] + ag);
        if ccRed in Channels then pc.r := IntToByte(Alpha[CurBits.r] + ag);
        Inc(pc);
      end;
      pc := Pointer(TCnNativeInt(pc) + Gap);
    end;
  end;
  Changed;
end;

// µ÷ÕûÍ¼Ïñ¶Ô±È¶È
procedure TCnBitmap.Contrast(Range: TAdjustRange; Channels: TColorChannels);
var
  x, y: Integer;
  Table: array[0..255] of Byte;
  CurBits: PCnColor;
  Amount: Integer;
begin
  if Empty or (Channels = []) then Exit;
  Changing;
  Amount := RangeToInt(Range, -256, 256);
  for x := 0 to 255 do
    Table[x] := IntToByte(x + (x - 128) * Amount div 256);
  CurBits := Bits;
  if Channels = csAllChannels then
  begin
    for y := 1 to FHeight do
    begin
      for x := 1 to FWidth do
      begin
        CurBits.b := Table[CurBits.b];
        CurBits.g := Table[CurBits.g];
        CurBits.r := Table[CurBits.r];
        Inc(CurBits);
      end;
      CurBits := Pointer(TCnNativeInt(CurBits) + Gap);
    end;
  end
  else
  begin
    for y := 1 to FHeight do
    begin
      for x := 1 to FWidth do
      begin
        if ccBlue in Channels then CurBits.b := Table[CurBits.b];
        if ccGreen in Channels then CurBits.g := Table[CurBits.g];
        if ccRed in Channels then CurBits.r := Table[CurBits.r];
        Inc(CurBits);
      end;
      CurBits := Pointer(TCnNativeInt(CurBits) + Gap);
    end;
  end;
  Changed;
end;

// µ÷Õûµ±Ç°Í¼ÏñµÄÉ«½×
// Ëã·¨Éè¼Æ£ºÖÜ¾¢Óğ
procedure TCnBitmap.Levels(InLow, InHigh, OutLow, OutHigh: Byte;
  Channels: TColorChannels);
var
  x, y: Integer;
  Table: array[0..255] of Byte;
  CurBits: PCnColor;
begin
  if Empty or (InHigh <= InLow) then
    Exit;

  Changing;
  for x := 0 to InLow - 1 do
    Table[x] := OutLow;
  for x := InHigh + 1 to 255 do
    Table[x] := OutHigh;
  for x := InLow to InHigh do
    Table[x] := (x - InLow) * (OutHigh - OutLow) div (InHigh - InLow) + OutLow;

  CurBits := Bits;
  if Channels = csAllChannels then
  begin
    for y := 1 to FHeight do
    begin
      for x := 1 to FWidth do
      begin
        CurBits.b := Table[CurBits.b];
        CurBits.g := Table[CurBits.g];
        CurBits.r := Table[CurBits.r];
        Inc(CurBits);
      end;
      CurBits := Pointer(TCnNativeInt(CurBits) + Gap);
    end;
  end
  else
  begin
    for y := 1 to FHeight do
    begin
      for x := 1 to FWidth do
      begin
        if ccBlue in Channels then CurBits.b := Table[CurBits.b];
        if ccGreen in Channels then CurBits.g := Table[CurBits.g];
        if ccRed in Channels then CurBits.r := Table[CurBits.r];
        Inc(CurBits);
      end;
      CurBits := Pointer(TCnNativeInt(CurBits) + Gap);
    end;
  end;
  Changed;
end;

// Í¼Ïñ»Ò¶È»¯
procedure TCnBitmap.Grayscale(Channels: TColorChannels);
var
  Grays: array[0..256] of Byte;
  I, x, y: Integer;
  CurBits: PCnColor;
begin
  if Empty then Exit;
  Changing;
  x := 0;
  y := 0;
  for I := 0 to 85 do
  begin
    Grays[x + 0] := y;        // Grays[i] := i div 3
    Grays[x + 1] := y;
    Grays[x + 2] := y;
    Inc(y);
    Inc(x, 3);
  end;
  CurBits := Bits;
  if Channels = csAllChannels then
  begin
    for y := 0 to FHeight - 1 do
    begin
      for x := 0 to FWidth - 1 do
      begin                   // Gray := (r + g + b) div 3
        I := Grays[CurBits.b] + Grays[CurBits.g] + Grays[CurBits.r];
        CurBits.b := I;
        CurBits.g := I;
        CurBits.r := I;
        Inc(CurBits);
      end;
      CurBits := Pointer(TCnNativeInt(CurBits) + Gap);
    end;
  end
  else
  begin
    for y := 0 to FHeight - 1 do
    begin
      for x := 0 to FWidth - 1 do
      begin                   // Gray := (r + g + b) div 3
        I := Grays[CurBits.b] + Grays[CurBits.g] + Grays[CurBits.r];
        if ccBlue in Channels then CurBits.b := I;
        if ccGreen in Channels then CurBits.g := I;
        if ccRed in Channels then CurBits.r := I;
        Inc(CurBits);
      end;
      CurBits := Pointer(TCnNativeInt(CurBits) + Gap);
    end;
  end;
  Changed;
end;

// ÑÕÉ«·´×ª
procedure TCnBitmap.Invert(Channels: TColorChannels);
var
  x, y: Integer;
  CurBits: PCnColor;
begin
  if Empty then Exit;
  Changing;
  CurBits := Bits;
  if Channels = csAllChannels then
  begin
    for y := 0 to FHeight - 1 do
    begin
      for x := 0 to Width - 1 do
      begin
        CurBits.b := not CurBits.b; // ÑÕÉ«È¡·´
        CurBits.g := not CurBits.g; // Ò»Ìõ»ã±àÖ¸ÁîÍê³É
        CurBits.r := not CurBits.r;
        Inc(CurBits);
      end;
      CurBits := Pointer(TCnNativeInt(CurBits) + Gap);
    end;
  end
  else
  begin
    for y := 0 to FHeight - 1 do
    begin
      for x := 0 to Width - 1 do
      begin
        if ccBlue in Channels then CurBits.b := not CurBits.b; // ÑÕÉ«È¡·´
        if ccGreen in Channels then CurBits.g := not CurBits.g; // Ò»Ìõ»ã±àÖ¸ÁîÍê³É
        if ccRed in Channels then CurBits.r := not CurBits.r;
        Inc(CurBits);
      end;
      CurBits := Pointer(TCnNativeInt(CurBits) + Gap);
    end;
  end;
  Changed;
end;

// Í¼Ïñ°´Ö¸¶¨ÑÕÉ«²ÊÉ«»¯£¨Ö§³ÖÍ¸Ã÷·½Ê½£©
procedure TCnBitmap.Colorize(Color: TColor);
begin
  Colorize(CnColor(Color));
end;

procedure TCnBitmap.Colorize(Color: TCnColor);
var
  x, y: Integer;
  CurBits: PCnColor;
  Tran: Boolean;
  TranColor: TCnColor;
  ra, ga, ba: Byte;
begin
  if Empty then Exit;
  Changing;
  ra := Color.r;
  ga := Color.g;
  ba := Color.b;
  CurBits := Bits;
  Tran := FTransparent;
  TranColor := GetTranColor;
  for y := 0 to Height - 1 do
  begin
    for x := 0 to Width - 1 do
    begin
      with TranColor do
        if not Tran or (r <> CurBits.r) or (g <> CurBits.g) or (b <> CurBits.b) then
        begin
          CurBits.b := IntToByte((CurBits.b - 192) + ba);
          CurBits.g := IntToByte((CurBits.g - 192) + ga);
          CurBits.r := IntToByte((CurBits.r - 192) + ra);
        end;
      Inc(CurBits);
    end;
    CurBits := Pointer(TCnNativeInt(CurBits) + Gap);
  end;
  Changed;
end;

//--------------------------------------------------------//
// Í¼Ïñ¼¸ºÎ±ä»»                                           //
// Ëã·¨À´Ô´£ºFastLib¡¢pnBitmap¡¢ÖÜ¾¢Óğ                    //
// Ëã·¨ĞŞ¸Ä£ºÖÜ¾¢Óğ                                       //
//--------------------------------------------------------//

// Í¼Ïñ´¹Ö±£¨Ë®Æ½£©·­×ª
procedure TCnBitmap.Flip(Horizontal: Boolean);
var
  w, h, x, y: Integer;
  CurBits: TCnColor;
  TmPLine, TmPLine2, Line: PCnLine;
  TopY: Integer;
begin
  if Empty then Exit;
  Changing;
  TmPLine := nil;
  w := FWidth - 1;
  h := FHeight - 1;

  TopY := FHeight - 1;
  if not Horizontal then
  begin
    TopY := h div 2;
    GetMem(TmPLine, RowInc);
  end;

  try
    Line := Bits;
    for y := 0 to TopY do
    begin
      if Horizontal then      // Ë®Æ½·­×ª
        for x := 0 to w div 2 do
        begin
          CurBits := Line[x];
          Line[x] := Line[w - x];
          Line[w - x] := CurBits;
        end
      else
      begin                   // ´¹Ö±·­×ª
        TmPLine2 := Pointer(TCnNativeInt(Bits) + (h - y) * RowInc);
        CopyMemory(TmPLine, Line, RowInc);
        CopyMemory(Line, TmPLine2, RowInc);
        CopyMemory(TmPLine2, TmPLine, RowInc);
      end;
      Line := Pointer(TCnNativeInt(Line) + RowInc);
    end;
  finally
    if not Horizontal then FreeMem(TmPLine);
  end;
  Changed;
end;

// Í¼ÏñĞı×ª
procedure TCnBitmap.Turn(Angle: TTurnAngle);
var
  Bmp: TCnBitmap;
  x, y, tx, ty: Integer;
  Dst: PCnLine;
begin
  Bmp := TCnBitmap.Create;
  try
    Bmp.Assign(Self);
    case Angle of
      ta90:
        begin
          SetSize(Height, Width);
          tx := Width - 1;
          for y := 0 to Height - 1 do
          begin
            Dst := FScanLine[y];
            for x := 0 to Width - 1 do
              Dst[x] := Bmp.FScanLine[tx - x][y];
          end;
        end;
      ta180:
        begin
          tx := Width - 1;
          ty := Height - 1;
          for y := 0 to Height - 1 do
          begin
            Dst := FScanLine[y];
            for x := 0 to Width - 1 do
              Dst[x] := Bmp.FScanLine[ty - y][tx - x];
          end;
        end;
      ta270:
        begin
          SetSize(Height, Width);
          ty := Height - 1;
          for y := 0 to Height - 1 do
          begin
            Dst := FScanLine[y];
            for x := 0 to Width - 1 do
              Dst[x] := Bmp.FScanLine[x][ty - y];
          end;
        end;
    end;
  finally
    Bmp.Free;
  end;
end;

// Ë®Æ½ÒÆ¶¯Í¼Ïñ
procedure TCnBitmap.VShift(Amount: Integer);
var
  p, Line: Pointer;
  y: Integer;
begin
  if Empty then Exit;
  if Amount < 0 then Amount := Width - (Abs(Amount) mod Width);
  if Amount >= Width then Amount := Amount mod Width;
  if Amount = 0 then Exit;
  Changing;
  GetMem(Line, Amount * 3);   // ÁÙÊ±»º³åÇø
  try
    p := Bits;
    for y := 0 to Height - 1 do
    begin                     // ¸´ÖÆ×ó°ë²¿·Öµ½ÁÙÊ±»º³åÇø
      CopyMemory(Line, Pointer(TCnNativeInt(p) + ((Width - Amount) * 3)), Amount * 3);
      MoveMemory(Pointer(TCnNativeInt(p) + (Amount * 3)), p, (Width - Amount) * 3);
      CopyMemory(p, Line, Amount * 3);
      p := Pointer(TCnNativeInt(p) + RowInc);
    end;
  finally
    FreeMem(Line);
  end;
  Changed;
end;

// ´¹Ö±ÒÆ¶¯Í¼Ïñ
procedure TCnBitmap.HShift(Amount: Integer);
var
  Buff: Pointer;
  p, y: TCnNativeInt;
begin
  if Empty then
    Exit;

  if Amount < 0 then Amount := Height mod Abs(Amount);
  if Amount >= Height then Amount := Amount mod Height;
  if Amount = 0 then
    Exit;

  Changing;
  p := TCnNativeInt(Bits) + (Height * (Gap)) + ((Height * Width) * 3);
  p := p - TCnNativeInt(FScanLine[Amount]);
  y := TCnNativeInt(FScanLine[Amount]) - TCnNativeInt(Bits);
  GetMem(Buff, y);            // ÁÙÊ±»º³åÇø

  try
    CopyMemory(Buff, FScanLine[Height - Amount], y);
    MoveMemory(FScanLine[Amount], Bits, p);
    CopyMemory(Bits, Buff, y);
  finally
    FreeMem(Buff);
  end;
  Changed;
end;

// Î»Í¼Ğı×ª
// Ëã·¨Éè¼Æ£ºÖÜ¾¢Óğ 2002.01.27
// Ö§³Ö×îÁÚ½ü²åÖµËã·¨ºÍ¿ìËÙ¶ş´Î²åÖµËã·¨£¨SmoothFilter ÊôĞÔ£©
// Ö§³ÖÍêÕûµÄÍ¸Ã÷·½Ê½
// Angle: ½Ç¶È -360..360;
procedure TCnBitmap.Rotate(DstCenter: TPoint; Src: TCnBitmap; Angle: Double);
var
  FAngle: Double;
  ARect: TRect;
  iCos, iSin: Integer;
  SrcX, SrcY: Integer;
  px, py: Integer;
  x, y, x1, x2, y1, y2, xs, ys: Integer;
  zx, zy, izy: Integer;
  w1, w2, w3, w4: Integer;
  Col1, Col2, Col3, Col4, Dst: PCnColor;
  Tran: Boolean;
  TranColor: TCnColor;
begin
  if Empty or not Assigned(Src) or Src.Empty then Exit;
  if not GetRotateRect(ClientRect, DstCenter, Src.Width, Src.Height, Angle,
    ARect) then Exit;

  Changing;
  Tran := Src.Transparent;
  TranColor := Src.GetTranColor;
  FAngle := -Angle * Pi / 180; // ·´·½Ïò¼ÆËãÔ­Ê¼µã
  iSin := Round(Sin(FAngle) * $8000);
  iCos := Round(Cos(FAngle) * $8000);
  for y := ARect.Top to ARect.Bottom - 1 do
  begin
    py := y - DstCenter.y;
    for x := ARect.Left to ARect.Right - 1 do
    begin
      px := x - DstCenter.x;
      SrcX := px * iCos - py * iSin + Src.Width shl 14; // Ô­Ê¼µã
      SrcY := px * iSin + py * iCos + Src.Height shl 14;
      if not SmoothFilter then
      begin                   // ×îÁÚ½ü²åÖµËã·¨
        xs := SrcX + $3FFF;
        ys := SrcY + $3FFF;
        if (xs >= 0) and (ys >= 0) then
        begin
          xs := xs shr 15;
          ys := ys shr 15;
          if (xs < Src.Width) and (ys < Src.Height) then // ·¶Î§¼ì²â
          begin
            Col1 := @Src.FScanLine[ys][xs];
            if not Tran or (TranColor.b <> Col1.b) or (TranColor.g <> Col1.g) or
              (TranColor.r <> Col1.r) then // Í¸Ã÷¼ì²é
              FScanLine[y][x] := Col1^;
          end
        end
      end
      else
      begin                   // ¶ş´Î²åÖµËã·¨
        if SrcX > 0 then
          x1 := SrcX shr 15   // ×óÁÚ½üÏñËØ
        else
          x1 := -(-SrcX shr 15);
        x2 := x1 + 1;         // ÓÒÁÚ½üÏñËØ
        if SrcY > 0 then
          y1 := SrcY shr 15
        else
          y1 := -(-SrcY shr 15); // ÉÏÁÚ½üÉ¨ÃèÏß
        y2 := y1 + 1;         // ÏÂÁÚ½üÉ¨ÃèÏß
        if (x2 >= 0) and (x1 < Src.Width) and (y2 >= 0) and
          (y1 < Src.Height) then // ·¶Î§¼ì²â
        begin
          Dst := @FScanLine[y][x];
          if (x1 >= 0) and (y1 >= 0) then
          begin
            Col1 := @Src.FScanLine[y1][x1]; // ×óÉÏ½ÇÏñËØ
            if Tran and (TranColor.b = Col1.b) and (TranColor.g = Col1.g) and
              (TranColor.r = Col1.r) then // Í¸Ã÷¼ì²é
              Col1 := Dst;
          end
          else
            Col1 := Dst;
          if (x2 < Src.Width) and (y1 >= 0) then // ÓÒÉÏ½ÇÏñËØ
          begin
            Col2 := @Src.FScanLine[y1][x2]; // Í¸Ã÷¼ì²é
            if Tran and (TranColor.b = Col2.b) and (TranColor.g = Col2.g) and
              (TranColor.r = Col2.r) then
              Col2 := Dst;
          end
          else
            Col2 := Dst;
          if (x1 >= 0) and (y2 < Src.Height) then // ×óÏÂ½ÇÏñËØ
          begin
            Col3 := @Src.FScanLine[y2][x1]; // Í¸Ã÷¼ì²é
            if Tran and (TranColor.b = Col3.b) and (TranColor.g = Col3.g) and
              (TranColor.r = Col3.r) then
              Col3 := Dst;
          end
          else
            Col3 := Dst;
          if (x2 < Src.Width) and (y2 < Src.Height) then // ÓÒÏÂ½ÇÏñËØ
          begin
            Col4 := @Src.FScanLine[y2][x2];
            if Tran and (TranColor.b = Col4.b) and (TranColor.g = Col4.g) and
              (TranColor.r = Col4.r) then // Í¸Ã÷¼ì²é
              Col4 := Dst;
          end
          else
            Col4 := Dst;
          if (Col1 <> Dst) or (Col2 <> Dst) or (Col3 <> Dst) or (Col4 <> Dst) then
          begin               // ÖÁÉÙÓĞÒ»µã²»Í¸Ã÷
            zx := SrcX and $7FFF;
            zy := SrcY and $7FFF;
            izy := zy xor $7FFF;
            w2 := (zx * izy) shr 15; // ¼ÆËã¼ÓÈ¨Öµ
            w1 := izy - w2;
            w4 := (zx * zy) shr 15;
            w3 := zy - w4;
            Dst.b := (Col1.b * w1 + Col2.b * w2 + Col3.b * w3 + Col4.b * w4) shr 15;
            Dst.g := (Col1.g * w1 + Col2.g * w2 + Col3.g * w3 + Col4.g * w4) shr 15;
            Dst.r := (Col1.r * w1 + Col2.r * w2 + Col3.r * w3 + Col4.r * w4) shr 15;
          end;
        end;
      end;
    end;
  end;
  Changed;
end;

//--------------------------------------------------------//
// Í¼ÏñÂË¾µ´¦Àí                                           //
// Ëã·¨À´Ô´£ºFastLib¡¢pnBitmap                            //
// Ëã·¨ĞŞ¸Ä£ºÖÜ¾¢Óğ                                       //
//--------------------------------------------------------//

// 3X3¾í»ıÔËËã
// Ëã·¨ĞŞ¸Ä£ºÖÜ¾¢Óğ£¨Ê¹ÓÃÊı¾İ»º³åÇø´¦Àí¡¢ËÙ¶ÈÓÅ»¯Ô¼Ò»±¶£©
procedure TCnBitmap.ApplyFilter(Core: TFilterCore; Cent: Integer);
var
  x, y, Sum: Integer;
  Buff: Pointer;
  p1, p2, p3, Dst: PCnLine;
  y1, y2, y3: PCnLine;
  x1, x2, x3: Integer;
begin
  if Empty then Exit;
  Changing;
  GetMem(Buff, Size);
  try
    CopyMemory(Buff, Bits, Size);
    if Cent <> 0 then
      Sum := Cent
    else
    begin
      Sum := Core[0, 0] + Core[1, 0] + Core[2, 0] +
        Core[0, 1] + Core[1, 1] + Core[2, 1] +
        Core[0, 2] + Core[1, 2] + Core[2, 2];
      if Sum = 0 then Sum := 1;
    end;

    p1 := Pointer(TCnNativeInt(Buff) - RowInc);
    p2 := Buff;
    p3 := Pointer(TCnNativeInt(Buff) + RowInc);
    Dst := Bits;              // Ä¿±êÏñËØ
    for y := 0 to Height - 1 do
    begin
      if y > 0 then
        y1 := p1
      else
        y1 := p2;
      y2 := p2;
      if y < Height - 1 then
        y3 := p3
      else
        y3 := p2;
      for x := 0 to Width - 1 do
      begin
        if x > 0 then
          x1 := x - 1
        else
          x1 := 0;
        x2 := x;
        if x < Width - 1 then
          x3 := x + 1
        else
          x3 := x;
        Dst[x].b := IntToByte((y1[x1].b * Core[0, 0] + y1[x2].b * Core[0, 1] +
          y1[x3].b * Core[0, 2] + y2[x1].b * Core[1, 0] + y2[x2].b * Core[1, 1] +
          y2[x3].b * Core[1, 2] + y3[x1].b * Core[2, 0] + y3[x2].b * Core[2, 1] +
          y3[x3].b * Core[2, 2]) div Sum);
        Dst[x].g := IntToByte((y1[x1].g * Core[0, 0] + y1[x2].g * Core[0, 1] +
          y1[x3].g * Core[0, 2] + y2[x1].g * Core[1, 0] + y2[x2].g * Core[1, 1] +
          y2[x3].g * Core[1, 2] + y3[x1].g * Core[2, 0] + y3[x2].g * Core[2, 1] +
          y3[x3].g * Core[2, 2]) div Sum);
        Dst[x].r := IntToByte((y1[x1].r * Core[0, 0] + y1[x2].r * Core[0, 1] +
          y1[x3].r * Core[0, 2] + y2[x1].r * Core[1, 0] + y2[x2].r * Core[1, 1] +
          y2[x3].r * Core[1, 2] + y3[x1].r * Core[2, 0] + y3[x2].r * Core[2, 1] +
          y3[x3].r * Core[2, 2]) div Sum);
      end;
      p1 := Pointer(TCnNativeInt(p1) + RowInc);
      p2 := Pointer(TCnNativeInt(p2) + RowInc);
      p3 := Pointer(TCnNativeInt(p3) + RowInc);
      Dst := Pointer(TCnNativeInt(Dst) + RowInc);
    end;
  finally
    FreeMem(Buff);
  end;
  Changed;
end;

// Ä£ºı´¦Àí£¨µÍÍ¨ÂË²¨£©
procedure TCnBitmap.Blur;
var
  p1, p2, p3, Dst: PCnLine;
  x1, x2, x3: Integer;
  x, y: Integer;
  Bmp: TCnBitmap;
begin
  if Empty then Exit;
  Changing;
  Bmp := TCnBitmap.Create;
  try
    Bmp.SetSize(ClientRect);
    Move(FBits^, Bmp.FBits^, FSize);
    for y := 0 to FHeight - 1 do
    begin
      p1 := Bmp.FScanLine[TrimInt(y - 1, 0, FHeight - 1)];
      p2 := Bmp.FScanLine[y];
      p3 := Bmp.FScanLine[TrimInt(y + 1, 0, FHeight - 1)];
      Dst := FScanLine[y];
      for x := 0 to FWidth - 1 do
      begin
        x1 := TrimInt(x - 1, 0, FWidth - 1);
        x2 := x;
        x3 := TrimInt(x + 1, 0, FWidth - 1);
        Dst[x].b := (p1[x1].b shr 1 + p1[x2].b + p1[x3].b shr 1 + p2[x1].b +
          p2[x2].b shl 1 + p2[x3].b + p3[x1].b shr 1 + p3[x2].b +
          p3[x3].b shr 1) shr 3;
        Dst[x].g := (p1[x1].g shr 1 + p1[x2].g + p1[x3].g shr 1 + p2[x1].g +
          p2[x2].g shl 1 + p2[x3].g + p3[x1].g shr 1 + p3[x2].g +
          p3[x3].g shr 1) shr 3;
        Dst[x].r := (p1[x1].r shr 1 + p1[x2].r + p1[x3].r shr 1 + p2[x1].r +
          p2[x2].r shl 1 + p2[x3].r + p3[x1].r shr 1 + p3[x2].r +
          p3[x3].r shr 1) shr 3;
      end;
    end;
  finally
    Bmp.Free;
  end;
  Changed;
end;

// ·ÖÁÑÄ£ºı
procedure TCnBitmap.SplitBlur(Amount: Integer);
var
  Lin1, Lin2: PCnLine;
  pc: PCnColor;
  cx, x, y: Integer;
  Buf: array[0..3] of TCnColor;
begin
  if Empty then Exit;
  Changing;
  pc := Bits;
  for y := 0 to FHeight - 1 do
  begin
    Lin1 := FScanLine[TrimInt(y + Amount, 0, FHeight - 1)];
    Lin2 := FScanLine[TrimInt(y - Amount, 0, FHeight - 1)];
    for x := 0 to FWidth - 1 do
    begin
      cx := TrimInt(x + Amount, 0, FWidth - 1);
      Buf[0] := Lin1[cx];
      Buf[1] := Lin2[cx];
      cx := TrimInt(x - Amount, 0, Width - 1);
      Buf[2] := Lin1[cx];
      Buf[3] := Lin2[cx];
      pc.b := (Buf[0].b + Buf[1].b + Buf[2].b + Buf[3].b) shr 2;
      pc.g := (Buf[0].g + Buf[1].g + Buf[2].g + Buf[3].g) shr 2;
      pc.r := (Buf[0].r + Buf[1].r + Buf[2].r + Buf[3].r) shr 2;
      Inc(pc);
    end;
    pc := Pointer(TCnNativeInt(pc) + Gap);
  end;
  Changed;
end;

// ¿ìËÙ¸ßË¹Ä£ºı
procedure TCnBitmap.GaussianBlur(Amount: Integer);
var
  I: Integer;
begin
  if Empty or (Amount <= 0) then Exit;
  BeginUpdate;
  try
    for I := Amount downto 1 do
      SplitBlur(I);
  finally
    EndUpdate;
  end;
end;

// Èñ»¯´¦Àí£¨¸ßÍ¨ÂË²¨£©
procedure TCnBitmap.Sharpen;
begin
  SplitSharpen(1);
end;

// ·ÖÁÑÈñ»¯
procedure TCnBitmap.SplitSharpen(Amount: Integer);
var
  Lin0, Lin1, Lin2: PCnLine;
  pc: PCnColor;
  cx, x, y: Integer;
  Buf: array[0..8] of TCnColor;
begin
  if Empty then Exit;
  Changing;
  pc := Bits;
  for y := 0 to FHeight - 1 do
  begin
    Lin0 := FScanLine[TrimInt(y - Amount, 0, Height - 1)];
    Lin1 := FScanLine[y];
    Lin2 := FScanLine[TrimInt(y + Amount, 0, FHeight - 1)];
    for x := 0 to FWidth - 1 do
    begin
      cx := TrimInt(x - Amount, 0, FWidth - 1);
      Buf[0] := Lin0[cx];
      Buf[1] := Lin1[cx];
      Buf[2] := Lin2[cx];
      Buf[3] := Lin0[x];
      Buf[4] := Lin1[x];
      Buf[5] := Lin2[x];
      cx := TrimInt(x + Amount, 0, FWidth - 1);
      Buf[6] := Lin0[cx];     // ¾í»ıºË£º-1/8 -1/8 -1/8
      Buf[7] := Lin1[cx];     //         -1/8   2  -1/8
      Buf[8] := Lin2[cx];     //         -1/8 -1/8 -1/8
      pc.b := IntToByte((16 * Buf[4].b - (Buf[0].b + Buf[1].b + Buf[2].b +
        Buf[3].b + Buf[5].b + Buf[6].b + Buf[7].b + Buf[8].b)) div 8);
      pc.g := IntToByte((16 * Buf[4].g - (Buf[0].g + Buf[1].g + Buf[2].g +
        Buf[3].g + Buf[5].g + Buf[6].g + Buf[7].g + Buf[8].g)) div 8);
      pc.r := IntToByte((16 * Buf[4].r - (Buf[0].r + Buf[1].r + Buf[2].r +
        Buf[3].r + Buf[5].r + Buf[6].r + Buf[7].r + Buf[8].r)) div 8);
      Inc(pc);
    end;
    pc := Pointer(TCnNativeInt(pc) + Gap);
  end;
  Changed;
end;

// ÔöÇ¿Èñ»¯
procedure TCnBitmap.SharpenMore(Amount: Integer);
var
  I: Integer;
begin
  if Empty or (Amount <= 0) then Exit;
  BeginUpdate;
  try
    for I := Amount downto 1 do
      SplitSharpen(I);
  finally
    EndUpdate;
  end;
end;

// Åç½¦Ğ§¹û
procedure TCnBitmap.Spray(Amount: Integer);
var
  r, x, y: Integer;
begin
  if Empty or (Amount <= 0) then Exit;
  Changing;
  for y := 0 to FHeight - 1 do
    for x := 0 to FWidth - 1 do
    begin
      r := Random(Amount);
      FScanLine[y][x] := FScanLine
        [TrimInt(y + (r - Random(r * 2)), 0, FHeight - 1)]
        [TrimInt(x + (r - Random(r * 2)), 0, FWidth - 1)];
    end;
  Changed;
end;

// ¸¡µñĞ§¹û
procedure TCnBitmap.Emboss;
var
  x, y: Integer;
  p1, p2: PCnColor;
  Line: PPCnLines;
begin
  if Empty then Exit;
  Changing;
  p1 := Bits;                 // µÚÒ»ĞĞ
  p2 := Pointer(TCnNativeInt(p1) + RowInc + 3); // ÓÒÏÂ¸÷¼ÓÒ»ÏñËØ
  GetMem(Line, RowInc);       // ÁÙÊ±ĞĞ±£´æ×îºóÒ»ĞĞÉ¨ÃèÏßÄÚÈİ
  try
    CopyMemory(Line, FScanLine[FHeight - 1], RowInc);
    for y := 0 to Height - 1 do
    begin
      for x := 0 to Width - 1 do
      begin
        p1.b := (p1.b + not p2.b) shr 1; // µ±Ç°ÏñËØÓëÓÒÏÂ½ÇÏñËØÈ¡·´µÄÆ½¾ùÖµ
        p1.g := (p1.g + not p2.g) shr 1;
        p1.r := (p1.r + not p2.r) shr 1;
        Inc(p1);
        if (y < FHeight - 2) and (x < FWidth - 2) then Inc(p2);
      end;
      p1 := Pointer(TCnNativeInt(p1) + FGap);
      if y < FHeight - 2 then // ×îºóÁ½ĞĞ
        p2 := Pointer(TCnNativeInt(p2) + Gap + 6)
      else
        p2 := Pointer(TCnNativeInt(Line) + 3);
    end;
  finally
    FreeMem(Line);
  end;
  Changed;
end;

// ±Ú»­Ğ§¹û
procedure TCnBitmap.Posterize(Amount: Integer);
var
  x, y: Integer;
  CurBits: PCnColor;
  Table: array[0..255] of Byte;
begin
  if Empty or (Amount <= 0) then Exit;
  Changing;
  for x := 0 to 255 do
    Table[x] := IntToByte(Round(x / Amount) * Amount);
  CurBits := Bits;
  for y := 0 to Height - 1 do
  begin
    for x := 0 to Width - 1 do
    begin
      CurBits.b := Table[CurBits.b];
      CurBits.g := Table[CurBits.g];
      CurBits.r := Table[CurBits.r];
      Inc(CurBits);
    end;
    CurBits := Pointer(TCnNativeInt(CurBits) + Gap);
  end;
  Changed;
end;

// µØÍ¼Ğ§¹û
procedure TCnBitmap.HeightMap(Amount: Integer);
var
  x, y, c: Integer;
  Src: PCnColor;
  Bmp: TCnBitmap;
  Table: array[0..765] of Byte;
begin
  if Empty or (Amount <= 0) then Exit;
  Changing;
  Bmp := TCnBitmap.Create;
  try
    Bmp.Assign(Self);
    for x := Low(Table) to High(Table) do
      Table[x] := x * Amount shr 8 div 3;
    Src := Bmp.Bits;
    for y := 0 to Height - 1 do
    begin
      for x := 0 to Width - 1 do
      begin
        c := y - Table[Src.b + Src.g + Src.r];
        if c >= 0 then FScanLine[c][x] := Src^;
        Inc(Src);
      end;
      Src := Pointer(TCnNativeInt(Src) + Gap);
    end;
  finally
    Bmp.Free;
    Changed;
  end;
end;

// Ë®µÎĞ§¹û
procedure TCnBitmap.Marble(Scale: Double; Turbulence: Integer);
var
  x, xm, y, ym: Integer;
  xx, yy: Double;
  Src: PCnColor;
  Bmp: TCnBitmap;
  Buf: array of Double;
begin
  if Empty or (Scale <= 0) or (Turbulence <= 0) then Exit;
  Changing;
  Bmp := TCnBitmap.Create;
  try
    Bmp.Assign(Self);
    SetLength(Buf, Turbulence);
    for x := 0 to Turbulence - 1 do
    begin
      Buf[x] := -Scale * Sin(x / Scale);
    end;

    Src := Bmp.Bits;
    for y := 0 to Height - 1 do
    begin
      yy := Scale * Cos((y mod Turbulence) / Scale);
      for x := 0 to Width - 1 do
      begin
        xx := Buf[x mod Turbulence];
        xm := Round(Abs(x + xx + yy));
        ym := Round(Abs(y + yy + xx));
        if (ym > 0) and (ym < Height) and (xm > 0) and (xm < Width) then
          FScanLine[ym][xm] := Src^;
        Inc(Src);
      end;
      Src := Pointer(TCnNativeInt(Src) + Gap);
    end;
  finally
    Buf := nil;
    Bmp.Free;
    Changed;
  end;
end;

// ²¨ÀËĞ§¹û
procedure TCnBitmap.Wave(XDiv, YDiv, RatioVal: Double; Wrap: Boolean);
type
  TArray = array[0..0] of Integer;
  PArray = ^TArray;
var
  I, J, XSrc, YSrc: Integer;
  st: PArray;
  Pix: PCnColor;
  Line: PCnLine;
  Dst: TCnBitmap;
  Max: TCnNativeInt;
  PInt: PInteger;
begin
  if Empty or (YDiv <= 0) or (XDiv <= 0) or (RatioVal <= 0) then
    Exit;

  Changing;
  Line := nil;
  Max := 0;
  st := nil;
  Dst := TCnBitmap.Create;
  try
    Dst.LoadBlank(FWidth, FHeight);
    GetMem(st, 4 * FHeight);
    for J := 0 to FHeight - 1 do
      st[J] := Round(RatioVal * Sin(J / YDiv));

    if Wrap then
      Max := TCnNativeInt(FScanLine[FHeight - 1]) + RowInc;

    for I := 0 to FWidth - 1 do
    begin
      YSrc := Round(RatioVal * Sin(I / XDiv));

      if Wrap then
      begin
        if YSrc < 0 then
          YSrc := FHeight - 1 - (-YSrc mod FHeight)
        else if YSrc >= FHeight then
          YSrc := YSrc mod (FHeight - 1);
      end;

      Pix := Pointer(TCnNativeInt(Dst.Bits) + I * 3);
      if ((YSrc >= 0) and (YSrc < FHeight)) or Wrap then
        Line := FScanLine[YSrc];
      PInt := PInteger(st);

      for J := 0 to FHeight - 1 do
      begin
        if Wrap then
        begin
          XSrc := I + PInt^;
          Inc(PInt);
          if XSrc < 0 then
            XSrc := FWidth - 1 - (-XSrc mod FWidth)
          else if XSrc >= FWidth then
            XSrc := XSrc mod FWidth;
          Pix^ := Line[XSrc];
          Pix := Pointer(TCnNativeInt(Pix) + Dst.RowInc);
          Line := Pointer(TCnNativeInt(Line) + FRowInc);
          if TCnNativeInt(Line) >= Max then
            Line := FBits;
        end
        else
        begin
          if (YSrc >= FHeight) then Break;
          XSrc := I + st[J];
          if (XSrc > -1) and (XSrc < FWidth) and (YSrc > -1) then
            Pix^ := Line^[XSrc]
          else if YSrc = -1 then
          begin
            Pix := Pointer(TCnNativeInt(Pix) + Dst.RowInc);
            Line := FBits;
            YSrc := 0;
            Continue;
          end;
          Pix := Pointer(TCnNativeInt(Pix) + Dst.RowInc);
          Line := Pointer(TCnNativeInt(Line) + RowInc);
          Inc(YSrc);
        end;
      end;
    end;
    CopyMemory(FBits, Dst.Bits, FSize);
  finally
    if st <> nil then
      FreeMem(st);
    Dst.Free;
  end;
  Changed;
end;

// ÂíÈü¿Ë»¯
procedure TCnBitmap.Mosaic(xAmount, yAmount: Integer);
var
  Delta: Integer;
  tx, ty, cx, cy, ix, iy, x, y: Integer;
  Col: TCnColor;
  Pix: PCnColor;
  Line: PCnLine;
begin
  if Empty or (xAmount < 1) or (yAmount < 1) then
    Exit;

  Changing;
  ix := (xAmount shr 1) + (xAmount and 1);
  iy := (yAmount shr 1) + (yAmount and 1);
  y := 0;
  while y < Height do
  begin
    x := 0;
    cy := y + iy;
    if cy >= Height then
      Line := FScanLine[Height - 1]
    else
      Line := FScanLine[cy];
    if y + yAmount - 1 >= Height then
      ty := Height - 1 - y
    else
      ty := yAmount;
    while x < Width do
    begin
      cx := x + ix;
      if cx >= Width then
        Col := Line[Width - 1]
      else
        Col := Line[cx];
      if x + xAmount - 1 >= Width then
        tx := Width - 1 - x
      else
        tx := xAmount;
      Delta := RowInc - tx * 3;
      Pix := PTR(TCnNativeInt(FScanLine[y]) + x * 3);
      for cy := 1 to ty do
      begin
        for cx := 1 to tx do
        begin
          Pix^ := Col;
          Inc(Pix);
        end;
        Pix := PTR(TCnNativeInt(Pix) + Delta);
      end;
      Inc(x, xAmount);
    end;
    Inc(y, yAmount);
  end;
  Changed;
end;

// ĞıÎĞĞ§¹û
procedure TCnBitmap.Twist(Amount: Integer);
var
  fxmid, fymid: Single;
  txmid, tymid: Single;
  fx, fy: Single;
  tx2, ty2: Single;
  r: Single;
  theta: Single;
  ifx, ify: Integer;
  dx, dy: Single;
  OFFSET: Single;
  ty, tx: Integer;
  weight_x, weight_y: array[0..1] of Single;
  Weight: Single;
  new_red, new_green: Integer;
  new_blue: Integer;
  total_red, total_green: Single;
  total_blue: Single;
  ix, iy: Integer;
  sli, slo: PCnLine;
  Buff: Pointer;
  BuffOff: TCnNativeInt;

  function ArcTan2(xt, yt: Single): Single;
  begin
    if xt = 0 then
      if yt > 0 then
        Result := Pi / 2
      else
        Result := -(Pi / 2)
    else
    begin
      Result := ArcTan(yt / xt);
      if xt < 0 then
        Result := Pi + ArcTan(yt / xt);
    end;
  end;

begin
  if Empty or (Amount <= 0) then Exit;
  Changing;
  GetMem(Buff, Size);
  try
    Move(Bits^, Buff^, Size);
    BuffOff := TCnNativeInt(Buff) - TCnNativeInt(Bits);
    OFFSET := -(Pi / 2);
    dx := Width - 1;
    dy := Height - 1;
    r := Sqrt(dx * dx + dy * dy);
    tx2 := r;
    ty2 := r;
    txmid := (Width - 1) / 2; //Adjust these to move center of rotation
    tymid := (Height - 1) / 2; //Adjust these to move ......
    fxmid := (Width - 1) / 2;
    fymid := (Height - 1) / 2;
    if tx2 >= Width then tx2 := Width - 1;
    if ty2 >= Height then ty2 := Height - 1;

    for ty := 0 to Round(ty2) do
    begin
      for tx := 0 to Round(tx2) do
      begin
        dx := tx - txmid;
        dy := ty - tymid;
        r := Sqrt(dx * dx + dy * dy);
        if r = 0 then
        begin
          fx := 0;
          fy := 0;
        end
        else
        begin
          theta := ArcTan2(dx, dy) - r / Amount - OFFSET;
          fx := r * Cos(theta);
          fy := r * Sin(theta);
        end;
        fx := fx + fxmid;
        fy := fy + fymid;

        ify := Trunc(fy);
        ifx := Trunc(fx);
                // Calculate the weights.
        if fy >= 0 then
        begin
          weight_y[1] := fy - ify;
          weight_y[0] := 1 - weight_y[1];
        end
        else
        begin
          weight_y[0] := -(fy - ify);
          weight_y[1] := 1 - weight_y[0];
        end;
        if fx >= 0 then
        begin
          weight_x[1] := fx - ifx;
          weight_x[0] := 1 - weight_x[1];
        end
        else
        begin
          weight_x[0] := -(fx - ifx);
          weight_x[1] := 1 - weight_x[0];
        end;

        if ifx < 0 then
          ifx := Width - 1 - (-ifx mod Width)
        else if ifx > Width - 1 then
          ifx := ifx mod Width;
        if ify < 0 then
          ify := Height - 1 - (-ify mod Height)
        else if ify > Height - 1 then
          ify := ify mod Height;

        total_red := 0.0;
        total_green := 0.0;
        total_blue := 0.0;
        for ix := 0 to 1 do
        begin
          for iy := 0 to 1 do
          begin
            if ify + iy < Height then
              sli := Pointer(TCnNativeInt(FScanLine[ify + iy]) + BuffOff)
            else
              sli := Pointer(TCnNativeInt(FScanLine[Height - ify - iy]) + BuffOff);
            if ifx + ix < Width then
            begin
              new_red := sli^[ifx + ix].r;
              new_green := sli^[ifx + ix].g;
              new_blue := sli^[ifx + ix].b;
            end
            else
            begin
              new_red := sli^[Width - ifx - ix].r;
              new_green := sli^[Width - ifx - ix].g;
              new_blue := sli^[Width - ifx - ix].b;
            end;
            Weight := weight_x[ix] * weight_y[iy];
            total_red := total_red + new_red * Weight;
            total_green := total_green + new_green * Weight;
            total_blue := total_blue + new_blue * Weight;
          end;
        end;
        slo := FScanLine[ty];
        slo^[tx].r := Round(total_red);
        slo^[tx].g := Round(total_green);
        slo^[tx].b := Round(total_blue);
      end;
    end;
  finally
    FreeMem(Buff);
  end;
  Changed;
end;

// ÔÚµ±Ç°Í¼ÏñÉÏ²úÉú¹âÕÕĞ§¹û
// Ëã·¨Éè¼Æ£ºÖÜ¾¢Óğ 2002.03.04
// Ê¹ÓÃÓÅ»¯Ëã·¨£¬ËÙ¶È½Ï¿ì
procedure TCnBitmap.Lighting(Center: TPoint; OffX, OffY: Integer;
  Angle: Double; Color: TColor; Amount: TCnAlpha);
var
  Col: PCnColor;
  FAlpha: Integer;
  ARect: TRect;
  Table: array[0..90] of Integer; // ¹âÕÕÍÖÔ²·¶Î§90¶ÈÄÚ°ë¾¶³¤¶È
  I, x, y: Integer;
  r, g, b: Byte;
  tx, ty, tz, ta, tb, tab: Double;
  Len, MLen, beta, Weight: Integer;
begin
  if Empty then Exit;
  FAlpha := AlphaToInt(Amount);
  if FAlpha = 0 then Exit;
  OffX := Abs(OffX);
  OffY := Abs(OffY);
  if (OffX = 0) or (OffY = 0) then Exit;
  if not GetRotateRect(ClientRect, Center, OffX * 2, OffY * 2, Angle, ARect) then Exit;

  Changing;
  ta := Sqr(OffX);
  tb := Sqr(OffY);
  tab := ta * tb;
  Table[0] := OffX;           // 0 ¶ÈÊ±Îª³¤Öá
  Table[90] := OffY;          // 90 ¶ÈÊ±Îª¶ÌÖá
  for I := 1 to 89 do
  begin
    tz := Tan(I * PI / 180);
    tx := Sqrt(tab / (tb + ta * Sqr(tz))); // i ¶ÈÊ± X ×ø±ê
    ty := tx * tz;            // i ¶ÈÊ± Y ×ø±ê
    Table[I] := Round(Hypot(tx, ty)); // ÖĞĞÄ¾à£¨i ¶È°ë¾¶£©
  end;

  DeRGB(Color, r, g, b);
  MLen := Max(OffX, OffY);
  for y := ARect.Top to ARect.Bottom - 1 do
  begin
    Col := @FScanLine[y, ARect.Left];
    for x := ARect.Left to ARect.Right - 1 do
    begin
      Len := Round(Hypot(x - Center.x, y - Center.y)); // ÖĞĞÄ¾à
      if Len < MLen then
      begin
        if Center.x = x then  // ´¹Ö±ÏßÉÏ
          beta := 90
        else if Center.y = y then // Ë®Æ½ÏßÉÏ
          beta := 0
        else
          beta := Round(ArcTan((y - Center.y) / (x - Center.x)) * 180 / PI);
        beta := Round(beta - Angle) mod 360; // µãÓëÖĞĞÄÁ¬ÏßÓëXÖáµÄ¼Ğ½Ç
        if beta < 0 then Inc(beta, 360);
        if beta > 270 then
          beta := 360 - beta
        else if beta > 180 then
          beta := beta - 180
        else if beta > 90 then
          beta := 180 - beta; // ±ä»»µ½ 90 ¶È·¶Î§ÄÚ
        if Len <= Table[beta] then
        begin                 // ¼ÓÈ¨ÖµÎªµãµ½ÖĞĞÄµÄ¾àÀëÓëÖĞĞÄµ½¸Ã·½ÏòÍÖÔ²ÖÜ¾àÀëÖ®±È
          Weight := FAlpha * (Table[beta] - Len) div Table[beta];
          Col.b := Col.b + (b - Col.b) * Weight shr 8;
          Col.g := Col.g + (g - Col.g) * Weight shr 8;
          Col.r := Col.r + (r - Col.r) * Weight shr 8;
        end;
      end;
      Inc(Col);
    end;
  end;
  Changed;
end;

// ¹âÕÕĞ§¹û
procedure TCnBitmap.Lighting(Rect: TRect; Data: TCnLighting);
var
  Center: TPoint;
  W, H: Integer;
begin
  if Empty or not Assigned(Data) then Exit;
  W := RectWidth(Rect);
  H := RectHeight(Rect);
  Center.x := (Rect.Left + Rect.Right) div 2 + Data.OffsetX * W div 100;
  Center.y := (Rect.Top + Rect.Bottom) div 2 + Data.OffsetY * H div 100;
  Lighting(Center, Data.Width * W div 100, Data.Height * H div 100, Data.Angle,
    Data.Color, Data.Alpha);
end;

// ×ª»»ÎªºÚ°×ÑÚÂëÍ¼
procedure TCnBitmap.Mask(MaskColor: TCnColor);
begin
  MaskEx(MaskColor, CnColor(0, 0, 0), CnColor(255, 255, 255));
end;

procedure TCnBitmap.Mask(MaskColor: TColor);
begin
  Mask(CnColor(MaskColor));
end;

// ×ª»»Îª¶şÉ«ÑÚÂëÍ¼
procedure TCnBitmap.MaskEx(MaskColor, InColor, BackColor: TCnColor);
var
  x, y: Integer;
  Col: PCnColor;
begin
  if Empty then Exit;
  Changing;
  Col := FBits;
  for y := 0 to FHeight - 1 do
  begin
    for x := 0 to FWidth - 1 do
    begin
      if (Col.r = MaskColor.r) and (Col.g = MaskColor.g) and (Col.b = MaskColor.b) then
        Col^ := InColor
      else
        Col^ := BackColor;
      Inc(Col);
    end;
    Col := Pointer(TCnNativeInt(Col) + FGap);
  end;
  Changed;
end;

procedure TCnBitmap.MaskEx(MaskColor, InColor, BackColor: TColor);
begin
  MaskEx(CnColor(MaskColor), CnColor(InColor), CnColor(BackColor));
end;

// Ôö¼Ó²ÊÉ«ÔëÉùµã
procedure TCnBitmap.AddColorNoise(Amount: Integer);
var
  x, y: Integer;
  pc: PCnColor;
begin
  if Empty then Exit;
  Changing;
  Amount := TrimInt(Amount, 0, 255);
  pc := Bits;
  for y := 0 to Height - 1 do
  begin
    for x := 0 to Width - 1 do
    begin                     // Ôö¼ÓËæ»úÉ«µã
      pc.b := IntToByte(pc.b + (Random(Amount) - (Amount shr 1)));
      pc.g := IntToByte(pc.g + (Random(Amount) - (Amount shr 1)));
      pc.r := IntToByte(pc.r + (Random(Amount) - (Amount shr 1)));
      Inc(pc);
    end;
    pc := Pointer(TCnNativeInt(pc) + Gap);
  end;
  Changed;
end;

// Ôö¼ÓºÚ°×ÔëÉùµã
procedure TCnBitmap.AddMonoNoise(Amount: Integer);
var
  x, y, a: Integer;
  pc: PCnColor;
begin
  if Empty then Exit;
  Changing;
  Amount := TrimInt(Amount, 0, 255);
  pc := Bits;
  for y := 0 to Height - 1 do
  begin
    for x := 0 to Width - 1 do
    begin                     // Ôö¼ÓËæ»ú»Ò¶Èµã
      a := Random(Amount) - (Amount shr 1);
      pc.b := IntToByte(pc.b + a);
      pc.g := IntToByte(pc.g + a);
      pc.r := IntToByte(pc.r + a);
      Inc(pc);
    end;
    pc := Pointer(TCnNativeInt(pc) + Gap);
  end;
  Changed;
end;

// ÒÆÈ¥ÔëÉùµã£¨ãĞÖµÆ½»¬Ëã×Ó 3x3 ¾í»ı£©
// Ëã·¨Éè¼Æ£ºÖÜ¾¢Óğ
procedure TCnBitmap.RemoveNoise(Amount: Integer);
var
  dr, dg, db: Byte;
  y1, y2, y3, Dst: PCnLine;
  Buff: Pointer;
  x, y: Integer;
begin
  if Empty then Exit;
  Changing;
  Amount := TrimInt(Amount, 0, 255);
  GetMem(Buff, Size);
  try
    CopyMemory(Buff, Bits, Size);
    y1 := Buff;
    y2 := Pointer(TCnNativeInt(y1) + RowInc);
    y3 := Pointer(TCnNativeInt(y2) + RowInc);
    Dst := FScanLine[1];
    for y := 1 to Height - 2 do
    begin
      for x := 1 to Width - 2 do
      begin                   // ÁÚ½ü°ËÏñËØÆ½¾ùÖµ
        db := (y1[x - 1].b + y1[x].b + y1[x + 1].b + y2[x - 1].b +
          y2[x + 1].b + y3[x - 1].b + y3[x].b + y3[x + 1].b) shr 3;
        dg := (y1[x - 1].g + y1[x].g + y1[x + 1].g + y2[x - 1].g +
          y2[x + 1].g + y3[x - 1].g + y3[x].g + y3[x + 1].g) shr 3;
        dr := (y1[x - 1].r + y1[x].r + y1[x + 1].r + y2[x - 1].r +
          y2[x + 1].r + y3[x - 1].r + y3[x].r + y3[x + 1].r) shr 3;
        if (db - Dst[x].b >= Amount) or (Dst[x].b - db <= Amount) or
          (dg - Dst[x].g >= Amount) or (Dst[x].g - dg <= Amount) or
          (dr - Dst[x].r >= Amount) or (Dst[x].r - dr <= Amount) then
        begin
          Dst[x].b := db;
          Dst[x].g := dg;
          Dst[x].r := dr;
        end;
      end;
      y1 := Pointer(TCnNativeInt(y1) + RowInc);
      y2 := Pointer(TCnNativeInt(y2) + RowInc);
      y3 := Pointer(TCnNativeInt(y3) + RowInc);
      Dst := Pointer(TCnNativeInt(Dst) + RowInc);
    end;
  finally
    FreeMem(Buff);
  end;
  Changed;
end;

// Ôö¼ÓÃÉ°åÉ«
procedure TCnBitmap.AddMiddleColor(Color: TColor);
begin
  AddMiddleColorEx(Color, ClientRect);
end;

// Ôö¼ÓÃÉ°åÉ«£¨Ö¸¶¨ÇøÓò£©
procedure TCnBitmap.AddMiddleColorEx(Color: TColor; Rect: TRect);
var
  I, J: Integer;
  r, g, b: Byte;
  pc: PCnColor;
  ARect: TRect;
  x, y, w, h: Integer;
begin
  if Empty then Exit;
  if not IntersectRect(ARect, Rect, ClientRect) then Exit;
  Changing;
  DeRect(ARect, x, y, w, h);
  DeRGB(Color, r, g, b);
  for I := y to y + h - 1 do
  begin
    pc := @FScanLine[I][x];
    for J := x to x + w - 1 do
    begin
      pc.b := (pc.b + b) shr 1; // ÑÕÉ«Æ½¾ùÖµ
      pc.g := (pc.g + g) shr 1;
      pc.r := (pc.r + r) shr 1;
      Inc(pc);
    end;
  end;
  Changed;
end;

//--------------------------------------------------------//
// ÆäËüÍ¼Ïñ´¦Àí                                           //
// Ëã·¨À´Ô´£ºFastLib                                      //
// Ëã·¨ĞŞ¸Ä£ºÖÜ¾¢Óğ                                       //
//--------------------------------------------------------//

// ¸ù¾İËÄ½ÇÑÕÉ«ÖµÓÃ½¥±äÉ«Ìî³ä¾ØĞÎ
// ( c[0,0]    c[1,0]
//   c[0,1]    c[1,1] )
procedure TCnBitmap.InterpolateRect(Rect: TRect; c00, c10, c01, c11: TColor);
begin
  InterpolateRect(Rect, CnColor(c00), CnColor(c10),
    CnColor(c01), CnColor(c11));
end;

procedure TCnBitmap.InterpolateRect(Rect: TRect; c00, c10, c01, c11: TCnColor);
var
  xCount, yCount: Integer;
  t, t2, z, iz: Integer;
  rp, rp2, gp: Integer;
  gp2, bp, bp2: Integer;
  xx, dx: Integer;
  x1, x2, y1, y2: Integer;
  pb: PCnColor;
  ARect: TRect;
begin
  if Empty then Exit;
  if not IntersectRect(ARect, ClientRect, Rect) then Exit;
  Changing;
  x1 := ARect.Left;
  y1 := ARect.Top;
  x2 := ARect.Right;
  y2 := ARect.Bottom;
  z := 0;
  iz := $100000;
  if x2 <> x1 then
    t := $100000 div (x2 - x1)
  else
    t := 0;
  if y2 <> y1 then
    t2 := $100000 div (y2 - y1)
  else
    t2 := 0;
  dx := x2 - x1;
  for yCount := y1 to y2 do
  begin
    xx := ((c00.r * iz + c01.r * z) shr 20);
    rp := xx shl 20;          // ÆğÊ¼Öµ£¨´¹Ö±·½ÏòÒÑ½¥±ä£©
    rp2 := (((c10.r * iz + c11.r * z) shr 20) - xx) * t; // Ë®Æ½ÔöÁ¿
    xx := ((c00.g * iz + c01.g * z) shr 20);
    gp := xx shl 20;
    gp2 := (((c10.g * iz + c11.g * z) shr 20) - xx) * t;
    xx := ((c00.b * iz + c01.b * z) shr 20);
    bp := xx shl 20;
    bp2 := (((c10.b * iz + c11.b * z) shr 20) - xx) * t;
    pb := @FScanLine[yCount][x1];
    for xCount := 0 to dx do
    begin
      pb.b := bp shr 20;
      Inc(bp, bp2);
      pb.g := gp shr 20;
      Inc(gp, gp2);
      pb.r := rp shr 20;
      Inc(rp, rp2);
      Inc(pb);
    end;
    Inc(z, t2);
    Dec(iz, t2);
  end;
  Changed;
end;

//--------------------------------------------------------//
// ¿¹¾â³İ»­±Ê»æÖÆ·½·¨£¨Ö§³ÖĞ¡Êı£©                         //
// Ëã·¨Éè¼Æ£ºÖÜ¾¢Óğ                                       //
// Ëã·¨²Î¿¼£ºGraphic32¡¢FastLib                           //
//--------------------------------------------------------//

// È¡¸¡µãÊıÏñËØÑÕÉ«
function TCnBitmap.GetPixelsF(x, y: Single): TCnColor;
begin
  if Empty then
    raise EBitmapIsEmpty.Create(SBitmapIsEmpty);
  if (x < 0) or (x > Width - 1) or (y < 0) or (y > Height - 1) then
    raise EInvalidPixel.CreateFmt(SInvalidPixelF, [x, y])
  else
    Result := DoGetPixelF(Round(x * $8000), Round(y * $8000));
end;

// Ğ´¸¡µãÊıÏñËØÑÕÉ«
procedure TCnBitmap.SetPixelsF(x, y: Single; const Value: TCnColor);
begin
  if Empty then
    raise EBitmapIsEmpty.Create(SBitmapIsEmpty);
  if (x < 0) or (x > Width - 1) or (y < 0) or (y > Height - 1) then
    raise EInvalidPixel.CreateFmt(SInvalidPixelF, [x, y])
  else
    DoSetPixelF(Round(x * $8000), Round(y * $8000), Value);
end;

// È¡Ğ¡Êı£¨³Ë $7FFF£©µãÏñËØ
function TCnBitmap.DoGetPixelF(x, y: Integer): TCnColor;
var
  x1, x2, y1, y2: Integer;
  zx, zy, izy: Integer;
  w1, w2, w3, w4: Integer;
  Col1, Col2, Col3, Col4: PCnColor;
begin
  x1 := x shr 15;
  x2 := x1 + 1;
  y1 := y shr 15;
  y2 := y1 + 1;
  if (x1 < 0) or (x2 >= Width) or (y1 < 0) or (y2 >= Height) then Exit;
  zx := x and $7FFF;
  zy := y and $7FFF;
  izy := zy xor $7FFF;
  w2 := (zx * izy) shr 15;    // ¼ÆËã¼ÓÈ¨Öµ
  w1 := izy - w2;
  w4 := (zx * zy) shr 15;
  w3 := zy - w4;
  Col1 := @FScanLine[y1][x1];
  Col2 := @FScanLine[y1][x2];
  Col3 := @FScanLine[y2][x1];
  Col4 := @FScanLine[y2][x2];
  Result.b := (Col1.b * w1 + Col2.b * w2 + Col3.b * w3 + Col4.b * w4) shr 15;
  Result.g := (Col1.g * w1 + Col2.g * w2 + Col3.g * w3 + Col4.g * w4) shr 15;
  Result.r := (Col1.r * w1 + Col2.r * w2 + Col3.r * w3 + Col4.r * w4) shr 15;
end;

// Ğ´Ğ¡Êı£¨³Ë $7FFF£©µãÏñËØ
procedure TCnBitmap.DoSetPixelF(x, y: Integer; ARGB: TCnColor);
var
  x1, x2, y1, y2: Integer;
  zx, zy, izy: Integer;
  w1, w2, w3, w4: Integer;
begin
  x1 := x shr 15;
  x2 := x1 + 1;
  y1 := y shr 15;
  y2 := y1 + 1;
  if (x2 < 0) or (x1 >= Width) or (y2 < 0) or (y1 >= Height) then Exit;
  zx := x and $7FFF;
  zy := y and $7FFF;
  izy := zy xor $7FFF;
  w2 := (zx * izy) shr 15;    // ¼ÆËã¼ÓÈ¨Öµ
  w1 := izy - w2;
  w4 := (zx * zy) shr 15;
  w3 := zy - w4;
  if (y1 >= 0) and (x1 >= 0) then
    with FScanLine[y1][x1] do
    begin
      b := b + (ARGB.b - b) * w1 shr 15;
      g := g + (ARGB.g - g) * w1 shr 15;
      r := r + (ARGB.r - r) * w1 shr 15;
    end;
  if (y1 >= 0) and (x2 < Width) then
    with FScanLine[y1][x2] do
    begin
      b := b + (ARGB.b - b) * w2 shr 15;
      g := g + (ARGB.g - g) * w2 shr 15;
      r := r + (ARGB.r - r) * w2 shr 15;
    end;
  if (y2 < Height) and (x1 > 0) then
    with FScanLine[y2][x1] do
    begin
      b := b + (ARGB.b - b) * w3 shr 15;
      g := g + (ARGB.g - g) * w3 shr 15;
      r := r + (ARGB.r - r) * w3 shr 15;
    end;
  if (y2 < Height) and (x2 < Width) then
    with FScanLine[y2][x2] do
    begin
      b := b + (ARGB.b - b) * w4 shr 15;
      g := g + (ARGB.g - g) * w4 shr 15;
      r := r + (ARGB.r - r) * w4 shr 15;
    end;
end;

// ÏŞÖÆÖ±Ïß·¶Î§
// Ëã·¨À´Ô´£ºGraphic32
function TCnBitmap.ClipLineF(var X0, Y0, X1, Y1: Single;
  MinX, MaxX, MinY, MaxY: Single): Boolean;
type
  Edge = (Left, Right, Top, Bottom);
  OutCode = set of Edge;
var
  Accept, AllDone: Boolean;
  OutCode0, OutCode1, OutCodeOut: OutCode;
  x, y: Single;

  procedure CompOutCode(x, y: Single; var Code: OutCode);
  begin
    Code := [];
    if x < MinX then Code := Code + [Left];
    if x > MaxX then Code := Code + [Right];
    if y < MinY then Code := Code + [Top];
    if y > MaxY then Code := Code + [Bottom];
  end;

begin
  Accept := False;
  AllDone := False;
  CompOutCode(X0, Y0, OutCode0);
  CompOutCode(X1, Y1, OutCode1);
  repeat
    if (OutCode0 = []) and (OutCode1 = []) then // Íê³É
    begin
      Accept := True;
      AllDone := True;
    end
    else if (OutCode0 * OutCode1) <> [] then AllDone := True
    else                      // ¼ÆËã½»²æ
    begin
      if OutCode0 <> [] then OutCodeOut := OutCode0
      else OutCodeOut := OutCode1;
      x := 0;
      y := 0;
      if Left in OutCodeOut then
      begin
        y := Y0 + (Y1 - Y0) * (MinX - X0) / (X1 - X0);
        x := MinX;
      end
      else if Right in OutCodeOut then
      begin
        y := Y0 + (Y1 - Y0) * (MaxX - X0) / (X1 - X0);
        x := MaxX - 1;
      end
      else if Top in OutCodeOut then
      begin
        x := X0 + (X1 - X0) * (MinY - Y0) / (Y1 - Y0);
        y := MinY;
      end
      else if Bottom in OutCodeOut then
      begin
        x := X0 + (X1 - X0) * (MaxY - Y0) / (Y1 - Y0);
        y := MaxY;
      end;
      if OutCodeOut = OutCode0 then
      begin
        X0 := x;
        Y0 := y;
        CompOutCode(X0, Y0, OutCode0);
      end
      else
      begin
        X1 := x;
        Y1 := y;
        CompOutCode(X1, Y1, OutCode1);
      end
    end;
  until AllDone;
  Result := Accept;
end;

// »æÖÆÖ±Ïß
procedure TCnBitmap.DrawLineF(X1, Y1, X2, Y2: Single; Color: TColor);
var
  n, I: Integer;
  px, py, ex, ey, nx, ny, hyp: Integer;
  ARGB: TCnColor;
begin
  if not ClipLineF(x1, y1, x2, y2, 0, Width - 1, 0, Height - 1) then Exit;
  Changing;
  ARGB := CnColor(Color);
  px := Round(x1 * $8000);
  py := Round(y1 * $8000);
  ex := Round(x2 * $8000);
  ey := Round(y2 * $8000);
  nx := ex - px;              // ¿í¶È
  ny := ey - py;              // ¸ß¶È
  if (nx = 0) or (ny = 0) then
    hyp := Round(Hypot(nx, ny))
  else
    case FPenWeight of        // Ğ±±ß³¤
      pwThin: hyp := Round(Hypot(nx, ny));
      pwNormal: hyp := Round(Hypot(nx, ny) * 1.4); // ´ÖÏ¸ĞŞÕı
      pwThick: hyp := Round(Hypot(nx, ny) * 1.8);
    else hyp := Round(Hypot(nx, ny));
    end;

  if hyp < 256 then Exit;
  n := hyp shr 15;
  if n > 0 then
  begin
    nx := Round(nx / hyp * $8000);
    ny := Round(ny / hyp * $8000);
    for I := 0 to n - 1 do
    begin
      DoSetPixelF(px, py, ARGB); // »æÖÆµã
      px := px + nx;
      py := py + ny;
    end;
    DoSetPixelF(ex, ey, ARGB); // »æÖÆ¶Ëµã
  end;
  Changed;
end;

// »æÖÆµ½Ö¸¶¨µã
procedure TCnBitmap.LineToF(x, y: Single);
begin
  DrawLineF(FPenPosF.x, FPenPosF.y, x, y, FPenColor);
  MoveToF(x, y);
end;

// »æÖÆµ½Ö¸¶¨µã
procedure TCnBitmap.LineToF(Point: TPointF);
begin
  LineToF(Point.x, Point.y);
end;

// ÒÆ¶¯»­±Ê
procedure TCnBitmap.MoveToF(Point: TPointF);
begin
  FPenPosF := Point;
end;

// ÒÆ¶¯»­±Ê
procedure TCnBitmap.MoveToF(x, y: Single);
begin
  MoveToF(PointF(x, y));
end;

// »æÖÆ¾ØĞÎ
procedure TCnBitmap.DrawRectF(const Rect: TRectF);
begin
  with Rect do
  begin
    DrawLineF(Left, Top, Left, Bottom, FPenColor);
    DrawLineF(Left, Bottom, Right, Bottom, FPenColor);
    DrawLineF(Right, Bottom, Right, Top, FPenColor);
    DrawLineF(Right, Top, Left, Top, FPenColor);
  end;
end;

// »æÖÆÕÛÏß
procedure TCnBitmap.PolylineF(const Points: TPointFArray);
var
  I: Integer;
  SavePenPos: TPointF;
begin
  if Empty or (Length(Points) < 2) then Exit;
  BeginUpdate;
  try
    SavePenPos := FPenPosF;
    FPenPosF := Points[Low(Points)];
    for I := Low(Points) + 1 to High(Points) do
      LineToF(Points[I]);
    FPenPosF := SavePenPos;
  finally
    EndUpdate;
  end;
end;

// »æÖÆÍÖÔ²
procedure TCnBitmap.EllipseF(X1, Y1, X2, Y2: Single);
var
  x, y: Integer;
  a, b: Single;
  cx, cy: Single;
  xl, xr, yt, yb: Single;
  xl1, xr1, yt1, yb1: Single;
  tmp: Single;
begin
  Changing;
  a := Abs(x2 - x1) / 2;      // °ë¾¶
  b := Abs(y2 - y1) / 2;
  if a = 0 then
    DrawLineF(x1, y1, x1, y2, FPenColor) // Ë®Æ½Ïß
  else if b = 0 then
    DrawLineF(x1, y1, x2, y1, FPenColor) // ´¹Ö±Ïß
  else if a <= b then
  begin
    cx := (x1 + x2) / 2;      // ÖĞĞÄµã
    cy := (y1 + y2) / 2;

    xl1 := cx;
    xr1 := cx;
    yt1 := cy - b;
    yb1 := cy + b;
    for x := 1 to Ceil(a) do
    begin
      if x < a then           // °ë¾¶ÄÚ
      begin
        xl := cx - x;
        xr := cx + x;
        tmp := b * Sqrt(1 - Sqr(x / a));
      end else
      begin                   // ±ß½çµã
        xl := cx - a;
        xr := cx + a;
        tmp := 0;
      end;
      yt := cy - tmp;
      yb := cy + tmp;
      DrawLineF(xl1, yt1, xl, yt, FPenColor); // ×óÉÏ²¿·Ö
      DrawLineF(xl1, yb1, xl, yb, FPenColor); // ×óÏÂ²¿·Ö
      DrawLineF(xr1, yt1, xr, yt, FPenColor); // ÓÒÉÏ²¿·Ö
      DrawLineF(xr1, yb1, xr, yb, FPenColor); // ÓÒÏÂ²¿·Ö
      xl1 := xl;
      xr1 := xr;
      yt1 := yt;
      yb1 := yb;
    end;
  end
  else
  begin
    cx := (x1 + x2) / 2;      // ÖĞĞÄµã
    cy := (y1 + y2) / 2;

    yt1 := cy;
    yb1 := cy;
    xl1 := cx - a;
    xr1 := cx + a;
    for y := 1 to Ceil(b) do
    begin
      if y < b then           // °ë¾¶ÄÚ
      begin
        yt := cy - y;
        yb := cy + y;
        tmp := a * Sqrt(1 - Sqr(y / b));
      end else
      begin                   // ±ß½çµã
        yt := cy - b;
        yb := cy + b;
        tmp := 0;
      end;
      xl := cx - tmp;
      xr := cx + tmp;
      DrawLineF(xl1, yt1, xl, yt, FPenColor); // ×óÉÏ²¿·Ö
      DrawLineF(xl1, yb1, xl, yb, FPenColor); // ×óÏÂ²¿·Ö
      DrawLineF(xr1, yt1, xr, yt, FPenColor); // ÓÒÉÏ²¿·Ö
      DrawLineF(xr1, yb1, xr, yb, FPenColor); // ÓÒÏÂ²¿·Ö
      xl1 := xl;
      xr1 := xr;
      yt1 := yt;
      yb1 := yb;
    end;
  end;
  Changed;
end;

// »æÖÆÍÖÔ²
procedure TCnBitmap.EllipseF(const Rect: TRectF);
begin
  EllipseF(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
end;

//--------------------------------------------------------//
// Æ½»¬×ÖÌå»æÖÆ·½·¨                                       //
// Ëã·¨Éè¼Æ£ºÖÜ¾¢Óğ                                       //
// Ëã·¨À´Ô´£ºÆ½»¬ÌØĞ§×ÖÌå¿Ø¼ş°ü AAFont V2.36£¨ÖÜ¾¢Óğ£©  //
// Ô­Ê¼Ëã·¨£ºÀîÎÄËÉĞÖÌá¹©µÄ AAFont V1.2 1999.07.13        //
//           liwensong@hotmail.com                        //
//           http://member.netease.com/~lws               //
//--------------------------------------------------------//

// È¡ÎÄ±¾´óĞ¡
function TCnBitmap.TextExtent(const Text: string): TSize;
var
  ADC: HDC;
  SaveFont: HFont;
  SaveSize: Integer;
begin
  if Text = '' then
  begin
    Result := EnSize(0, 0);
    Exit;
  end;
  ADC := Windows.GetDC(0);
  SaveSize := Font.Size;
  Font.Size := Font.Size * Font.Scale;
  SaveFont := SelectObject(ADC, Font.Handle);
  Windows.GetTextExtentPoint(ADC, PChar(Text), Length(Text), Result);
  SelectObject(ADC, SaveFont);
  Font.Size := SaveSize;
  ReleaseDC(0, ADC);
  Result.cx := (Result.cx + Font.Scale - 1) div Font.Scale;
  Result.cy := (Result.cy + Font.Scale - 1) div Font.Scale;
  // ¼ÆËãÒõÓ°
  if fsShadow in Font.StyleEx then
  begin
    Inc(Result.cx, Abs(Font.Shadow.FOffsetX));
    Inc(Result.cy, Abs(Font.Shadow.FOffsetY));
  end;
  // Ğ±Ìå×Ö¿í¶ÈĞ£Õı
  if fsItalic in Font.Style then
    Inc(Result.cx, Round(Result.cx / Length(Text) * csItalicAdjust));
end;

// È¡ÎÄ±¾¸ß¶È
function TCnBitmap.TextHeight(const Text: string): Integer;
begin
  Result := TextExtent(Text).cy;
end;

// È¡ÎÄ±¾¿í¶È
function TCnBitmap.TextWidth(const Text: string): Integer;
begin
  Result := TextExtent(Text).cx;
end;

// ³õÊ¼»¯»Ò¶Èµ÷»¯°åÊı¾İ
procedure InitGrayPal;
var
  I: Integer;
begin
  GrayLogPal.lpal.palVersion := $300;
  GrayLogPal.lpal.palNumEntries := 256;
  for I := 0 to 255 do
  begin
    GrayLogPal.dummy[I].peRed := I;
    GrayLogPal.dummy[I].peGreen := I;
    GrayLogPal.dummy[I].peBlue := I;
    GrayLogPal.dummy[I].peFlags := 0;
  end;
end;

// ³õÊ¼»¯ÓÃÓÚÆ½»¬×ÖÌå»æÖÆµÄ»Ò¶ÈÍ¼
procedure TCnBitmap.InitGrayBmp;
begin
  if FGrayBmp = nil then
  begin
    FGrayBmp := TBitmap.Create;
    FGrayBmp.PixelFormat := pf8Bit;
    FGrayBmp.Canvas.Brush.Style := bsSolid;
    FGrayBmp.Canvas.Brush.Color := clBlack;
    FGrayBmp.Palette := CreatePalette(GrayLogPal.lpal);
  end;
  if FFontMask = nil then FFontMask := TCnFontMask.Create;
end;

// ÊÍ·ÅÓÃÓÚÆ½»¬×ÖÌå»æÖÆµÄ»Ò¶ÈÍ¼
procedure TCnBitmap.FreeGrayBmp;
var
  HPal: HPALETTE;
begin
  if FGrayBmp <> nil then
  begin
    HPal := FGrayBmp.Palette;
    FGrayBmp.Palette := 0;
    FreeAndNil(FGrayBmp);
    DeleteObject(HPal);
  end;
  if FFontMask <> nil then FreeAndNil(FFontMask);
end;

// »æÖÆÆ½»¬×ÖÌåÃÉ°å
procedure TCnBitmap.DrawFontMaskEx(const Text: string; Extend: TSize; Point: TPoint);
var
  I, J: Integer;
  pS1, pS2, pS3, pS4: PByteArray;
  pDst: PByteArray;
  GrayRowInc: Integer;
  x: Integer;
begin
  InitGrayBmp;
  FGrayBmp.Width := Extend.cx * Font.Scale;
  FGrayBmp.Height := Extend.cy * Font.Scale;
  FFontMask.SetSize(Extend.cx, Extend.cy);

  FGrayBmp.Canvas.Font.Assign(Font); // Éè¶¨×ÖÌå
  FGrayBmp.Canvas.Font.Height := FGrayBmp.Canvas.Font.Height * Font.Scale;
  FGrayBmp.Canvas.Font.Color := clWhite;
  Windows.FillRect(FGrayBmp.Canvas.Handle, Bounds(0, 0, FGrayBmp.Width,
    FGrayBmp.Height), 0);
  Windows.TextOut(FGrayBmp.Canvas.Handle, Point.x, Point.y, PChar(Text), Length(Text));

  GrayRowInc := (FGrayBmp.Width + 3) div 4 * 4; // É¨ÃèÏß¿í¶È
  pS1 := FGrayBmp.ScanLine[0]; // Ô´»Ò¶ÈÍ¼
  pS2 := PByteArray(TCnNativeInt(pS1) - GrayRowInc);
  pS3 := PByteArray(TCnNativeInt(pS2) - GrayRowInc);
  pS4 := PByteArray(TCnNativeInt(pS3) - GrayRowInc);
  pDst := FFontMask.ScanLine[0];
  // Ä¿±ê»Ò¶ÈÎªÔ´¾ØĞÎ¿éµÄÆ½¾ùÖµ
  case Font.Quality of
    fqHigh:
      begin                   // ¸ß¾«¶È 4x4 ²ÉÑù
        for I := 0 to Extend.cy - 1 do
        begin
          for J := 0 to Extend.cx - 1 do
          begin
            x := J * 4;
            pDst^[J] :=
              (pS1^[x] + pS1^[x + 1] + pS1^[x + 2] + pS1^[x + 3] +
              pS2^[x] + pS2^[x + 1] + pS2^[x + 2] + pS2^[x + 3] +
              pS3^[x] + pS3^[x + 1] + pS3^[x + 2] + pS3^[x + 3] +
              pS4^[x] + pS4^[x + 1] + pS4^[x + 2] + pS4^[x + 3]) shr 4;
          end;
          pS1 := PByteArray(TCnNativeInt(pS4) - GrayRowInc);
          pS2 := PByteArray(TCnNativeInt(pS1) - GrayRowInc);
          pS3 := PByteArray(TCnNativeInt(pS2) - GrayRowInc);
          pS4 := PByteArray(TCnNativeInt(pS3) - GrayRowInc);
          pDst := PByteArray(TCnNativeInt(pDst) + FFontMask.FRowInc);
        end;
      end;
    fqNormal:
      begin                   // ÆÕÍ¨¾«¶È 3x3 ²ÉÑù
        for I := 0 to Extend.cy - 1 do
        begin
          for J := 0 to Extend.cx - 1 do
          begin
            x := J * 3;
            pDst^[J] :=
              (pS1^[x] + pS1^[x + 1] + pS1^[x + 2] shr 1 +
              pS2^[x] + pS2^[x + 1] + pS2^[x + 2] +
              pS3^[x] shr 1 + pS3^[x + 1] + pS3^[x + 2]) shr 3;
          end;
          pS1 := PByteArray(TCnNativeInt(pS3) - GrayRowInc);
          pS2 := PByteArray(TCnNativeInt(pS1) - GrayRowInc);
          pS3 := PByteArray(TCnNativeInt(pS2) - GrayRowInc);
          pDst := PByteArray(TCnNativeInt(pDst) + FFontMask.FRowInc);
        end;
      end;
    fqLow:
      begin                   // µÍ¾«¶È 2x2 ²ÉÑù
        for I := 0 to Extend.cy - 1 do
        begin
          for J := 0 to Extend.cx - 1 do
          begin
            x := J * 2;
            pDst^[J] :=
              (pS1^[x] + pS1^[x + 1] +
              pS2^[x] + pS2^[x + 1]) shr 2;
          end;
          pS1 := PByteArray(TCnNativeInt(pS2) - GrayRowInc);
          pS2 := PByteArray(TCnNativeInt(pS1) - GrayRowInc);
          pDst := PByteArray(TCnNativeInt(pDst) + FFontMask.FRowInc);
        end;
      end;
    fqNone:
      begin                   // ÎŞÆ½»¬Ğ§¹û
        for I := 0 to Extend.cy - 1 do
        begin
          CopyMemory(pDst, pS1, Extend.cx);
          pS1 := PByteArray(TCnNativeInt(pS1) - GrayRowInc);
          pDst := PByteArray(TCnNativeInt(pDst) + FFontMask.FRowInc);
        end;
      end;
  end;
end;

// »æÖÆÆ½»¬×ÖÌåÃÉ°å
procedure TCnBitmap.DrawFontMask(const Text: string);
begin
  DrawFontMaskEx(Text, TextExtent(Text), Point(0, 0));
end;

// ¼ÆËãÒõÓ°Æ«ÒÆ
function TCnBitmap.GetShadowPoint: TPoint;
begin
  if fsShadow in Font.StyleEx then
  begin
    if Font.Shadow.OffsetX > 0 then
      Result.x := Font.Shadow.OffsetX
    else
      Result.x := 0;
    if Font.Shadow.OffsetY > 0 then
      Result.y := Font.Shadow.OffsetY
    else
      Result.y := 0;
  end
  else
  begin
    Result.x := 0;
    Result.y := 0;
  end;
end;

// ¼ÆËãÎÄ±¾Æ«ÒÆ
function TCnBitmap.GetTextPoint: TPoint;
begin
  if fsShadow in Font.StyleEx then
  begin
    if Font.Shadow.OffsetX < 0 then
      Result.x := Abs(Font.Shadow.OffsetX)
    else
      Result.x := 0;
    if Font.Shadow.OffsetY < 0 then
      Result.y := Abs(Font.Shadow.OffsetY)
    else
      Result.y := 0;
  end
  else
  begin
    Result.x := 0;
    Result.y := 0;
  end;
end;

// ÎÄ±¾°´Ç°¾°É«Óë±³¾°»ìºÏ
procedure TCnBitmap.FontMaskBlend(x, y: Integer; AColor: TColor; Alpha: TCnAlpha;
  Mask: TCnFontMask);
var
  r, b, g: Byte;
  Src: PByteArray;
  Dst: PCnLine;
  Weight: Byte;
  dx, dy, sx, sy, w, h: Integer;
  I, J: Integer;
  FAlpha: Integer;
begin
  if Empty or not CalcDrawRect(x, y, Rect(0, 0, Mask.Width, Mask.Height),
    Rect(0, 0, Mask.Width, Mask.Height), dx, dy, sx, sy, w, h) then Exit;
  FAlpha := AlphaToInt(Alpha);
  if FAlpha = 0 then Exit;

  DeRGB(AColor, r, g, b);     // É«²Ê·ÖÁ¿
  for J := 0 to h - 1 do
  begin
    Src := @Mask.ScanLine[sy + J][sx];
    Dst := @ScanLine[dy + J][dx];
    for I := 0 to w - 1 do
    begin
      Weight := Src[I] * FAlpha shr 8; // »ìºÏÏµÊı
      if Weight <> 0 then
      begin
        if Weight = 255 then
        begin                 // Ç°¾°É«
          Dst[I].b := b;
          Dst[I].g := g;
          Dst[I].r := r;
        end
        else
        begin                 // »ìºÏ
          Inc(Dst[I].b, Weight * (b - Dst[I].b) shr 8);
          Inc(Dst[I].g, Weight * (g - Dst[I].g) shr 8);
          Inc(Dst[I].r, Weight * (r - Dst[I].r) shr 8);
        end;
      end;
    end;
  end;
end;

// ÎÄ±¾°´ÎÆÀíÓë±³¾°»ìºÏ
procedure TCnBitmap.FontMaskBlendEx(x, y: Integer; Alpha: TCnAlpha;
  Mask: TCnFontMask; ForeBmp: TCnBitmap);
var
  Src: PByteArray;
  Fore, Dst: PCnLine;
  Weight: Byte;
  dx, dy, sx, sy, w, h: Integer;
  I, J: Integer;
  FAlpha: Integer;
begin
  if Empty or not CalcDrawRect(x, y, Rect(0, 0, Mask.Width, Mask.Height),
    Rect(0, 0, Mask.Width, Mask.Height), dx, dy, sx, sy, w, h) then Exit;

  if (ForeBmp.Width <> Mask.Width) or (ForeBmp.Height <> Mask.Height) then
    raise EInvalidForeBmp.Create(SInvalidForeBitmap); // ´íÎóµÄÎÆÀíÍ¼

  FAlpha := AlphaToInt(Alpha);
  if FAlpha = 0 then Exit;

  for J := 0 to h - 1 do
  begin
    Src := @Mask.ScanLine[J + sy][sx];
    Fore := @ForeBmp.ScanLine[J + sy][sx];
    Dst := @ScanLine[J + dy][dx];
    for I := 0 to w - 1 do
    begin
      Weight := Src[I] * FAlpha shr 8; // »ìºÏÏµÊı
      if Weight <> 0 then
      begin
        if Weight = 255 then
          Dst[I] := Fore[I]   // Ç°¾°É«
        else
        begin                 // »ìºÏ
          Inc(Dst[I].b, Weight * (Fore[I].b - Dst[I].b) shr 8);
          Inc(Dst[I].g, Weight * (Fore[I].g - Dst[I].g) shr 8);
          Inc(Dst[I].r, Weight * (Fore[I].r - Dst[I].r) shr 8);
        end;
      end;
    end;
  end;
end;

// Æ½»¬ÎÄ±¾Êä³ö
procedure TCnBitmap.TextOut(x, y: Integer; const Text: string);
var
  TextPoint, ShadowPoint: TPoint;
  ShadowMask: TCnFontMask;
  Fore: TCnBitmap;
  IsTexture, IsGrad, IsLight, IsNoise: Boolean;
  I, ABlur: Integer;
begin
  if Empty or (Text = '') then Exit;
  BeginUpdate;
  try
    if fsShadow in Font.StyleEx then // ÒõÓ°¼ÆËã
    begin
      TextPoint := GetTextPoint;
      ShadowPoint := GetShadowPoint;
      TextPoint.x := TextPoint.x + x;
      TextPoint.y := TextPoint.y + y;
      ShadowPoint.x := ShadowPoint.x + x;
      ShadowPoint.y := ShadowPoint.y + y;
    end
    else
    begin
      TextPoint := Point(x, y);
    end;

    DrawFontMask(Text);       // ´´½¨×ÖÌåÃÉ°å
    if fsOutline in Font.StyleEx then // ÂÖÀª»¯
      FFontMask.Outline;
    if fsSpray in Font.StyleEx then // Åç½¦Ğ§¹û
      FFontMask.Spray(Font.Spray);

    if not FontClear then     // ±³¾°²»Í¸Ã÷
      FillRect(Bounds(x, y, FFontMask.Width, FFontMask.Height), FontBkColor);

    if fsShadow in Font.StyleEx then // ÒõÓ°´¦Àí
    begin
      ShadowMask := TCnFontMask.Create; // ÒõÓ°ÃÉ°å
      try
        ABlur := Font.Shadow.Blur;
        if ABlur > 0 then     // ¿¼ÂÇÄ£ºı
        begin
          ShadowMask.SetSize(FFontMask.Width + 4 * ABlur, FFontMask.Height +
            4 * ABlur);
          with ShadowMask do
            FillChar(FBuff^, FRowInc * FHeight, 0);
          for I := 0 to FFontMask.FHeight - 1 do
            Move(FFontMask.ScanLine[I][0], ShadowMask.ScanLine[2 * ABlur + I][2 *
              ABlur],
                FFontMask.Width);
          ShadowMask.Blur(ABlur); // ÒõÓ°Ä£ºı
        end
        else
          FFontMask.CopyTo(ShadowMask);
        FontMaskBlend(ShadowPoint.x - 2 * ABlur, ShadowPoint.y - 2 * ABlur,
          Font.Shadow.Color,
          Font.Shadow.Alpha * Font.Alpha div csMaxAlpha, ShadowMask);
      finally
        ShadowMask.Free;
      end;
    end;

    IsTexture := (fsTexture in Font.StyleEx) and Assigned(Font.Texture.Graphic) and
      not Font.Texture.Graphic.Empty; // ×ÖÌåÎÆÀí
    IsGrad := fsGradient in Font.StyleEx; // ×ÖÌå½¥±ä
    IsLight := fsLighting in Font.StyleEx; // ¹âÕÕĞ§¹û
    IsNoise := (fsNoise in Font.StyleEx) and (Font.Noise > 0); // ÔëÉùĞ§¹û
    if IsTexture or IsGrad or IsLight or IsNoise then
    begin
      Fore := TCnBitmap.Create;
      try
        Fore.SetSize(FFontMask.Width, FFontMask.Height);
        if not IsGrad then
          Fore.Fill(Font.Color)  // ÎŞ½¥±äĞ§¹ûÊ±Ìî³ä×ÖÌåÉ«
        else if Font.Gradient.Style = gsRadial then
          Fore.Fill(Font.Gradient.ColorEnd);
        if IsTexture then
          Fore.DrawMode(Font.Texture.Graphic, Font.TextureMode)
        else if IsGrad then
          Fore.DrawGradient(Font.Gradient);
        if IsNoise then
          Fore.AddColorNoise(Font.Noise);
        if IsLight then
          Fore.Lighting(Fore.ClientRect, Font.Lighting);
        FontMaskBlendEx(TextPoint.x, TextPoint.y, Font.Alpha, FFontMask, Fore);
      finally
        Fore.Free;
      end;
    end
    else
      FontMaskBlend(TextPoint.x, TextPoint.y, Font.Color, Font.Alpha, FFontMask);
  finally
    EndUpdate;
  end;
end;

type
  TCnParentControl = class(TWinControl);

// ´Ó¸¸¿Ø¼ş¸´ÖÆ±³¾°¡£Õâ¸ö¹ı³ÌÀ´×Ô RxLibrary VCLUtils
procedure CopyControlParentImageToCanvas(AControl: TControl; Dest: TCanvas);
var
  I, Count, X, Y, SaveIndex: Integer;
  DC: HDC;
  R, SelfR, CtlR: TRect;
begin
  if AControl.Parent = nil then Exit;
  Count := AControl.Parent.ControlCount;
  DC := Dest.Handle;
  with AControl.Parent do
    ControlState := ControlState + [csPaintCopy];

  try
    SelfR := Bounds(AControl.Left, AControl.Top, AControl.Width, AControl.Height);
    X := -AControl.Left;
    Y := -AControl.Top;
    { Copy parent control image }
    SaveIndex := SaveDC(DC);
    try
      SetViewportOrgEx(DC, X, Y, nil);
      IntersectClipRect(DC, 0, 0, AControl.Parent.ClientWidth,
        AControl.Parent.ClientHeight);
      try
        with TCnParentControl(AControl.Parent) do
        begin
          Perform(WM_ERASEBKGND, DC, 0);
          PaintWindow(DC);
        end;
      except
        ;
      end;
    finally
      RestoreDC(DC, SaveIndex);
    end;
    { Copy images of graphic controls }
    for I := 0 to Count - 1 do
    begin
      if AControl.Parent.Controls[I] = AControl then
        Break
      else if (AControl.Parent.Controls[I] <> nil) and
        (AControl.Parent.Controls[I] is TGraphicControl) then
      begin
        with TGraphicControl(AControl.Parent.Controls[I]) do
        begin
          CtlR := Bounds(Left, Top, Width, Height);
          if Bool(IntersectRect(R, SelfR, CtlR)) and Visible then
          begin
            ControlState := ControlState + [csPaintCopy];
            SaveIndex := SaveDC(DC);
            try
              SetViewportOrgEx(DC, Left + X, Top + Y, nil);
              IntersectClipRect(DC, 0, 0, Width, Height);
              Perform(WM_PAINT, DC, 0);
            finally
              RestoreDC(DC, SaveIndex);
              ControlState := ControlState - [csPaintCopy];
            end;
          end;
        end;
      end;
    end;
  finally
    with AControl.Parent do
      ControlState := ControlState - [csPaintCopy];
  end;
end;

{ TCnColorSpaceConverter }

constructor TCnColorSpaceConverter.Create;
begin
  inherited Create;
end;

destructor TCnColorSpaceConverter.Destroy;
begin
  inherited Destroy;
end;

function TCnColorSpaceConverter.ClipByte(Value: Double): Byte;
begin
  if Value < 0 then
    Result := 0
  else if Value > 255 then
    Result := 255
  else
    Result := Round(Value);
end;

procedure TCnColorSpaceConverter.RGBToYCbCr(const R, G, B: Byte;
  out Y, Cb, Cr: Byte);
var
  YVal, CbVal, CrVal: Double;
begin
  // ITU-R BT.601 ±ê×¼×ª»»¹«Ê½
  // Y  =  0.299*R + 0.587*G + 0.114*B
  // Cb = -0.169*R - 0.331*G + 0.500*B + 128
  // Cr =  0.500*R - 0.419*G - 0.081*B + 128

  YVal := 0.299 * R + 0.587 * G + 0.114 * B;
  CbVal := -0.169 * R - 0.331 * G + 0.500 * B + 128;
  CrVal := 0.500 * R - 0.419 * G - 0.081 * B + 128;

  Y := ClipByte(YVal);
  Cb := ClipByte(CbVal);
  Cr := ClipByte(CrVal);
end;

procedure TCnColorSpaceConverter.YCbCrToRGB(const Y, Cb, Cr: Byte;
  out R, G, B: Byte);
var
  RVal, GVal, BVal: Double;
  CbAdj, CrAdj: Double;
begin
  // ITU-R BT.601 Äæ×ª»»¹«Ê½
  // R = Y + 1.402 * (Cr - 128)
  // G = Y - 0.344 * (Cb - 128) - 0.714 * (Cr - 128)
  // B = Y + 1.772 * (Cb - 128)

  CbAdj := Cb - 128;
  CrAdj := Cr - 128;

  RVal := Y + 1.402 * CrAdj;
  GVal := Y - 0.344 * CbAdj - 0.714 * CrAdj;
  BVal := Y + 1.772 * CbAdj;

  R := ClipByte(RVal);
  G := ClipByte(GVal);
  B := ClipByte(BVal);
end;

procedure TCnColorSpaceConverter.ConvertBitmapToYCbCr(Source: TBitmap;
  YPlane, CbPlane, CrPlane: TCnFloatMatrix);
var
  X, Y: Integer;
  P: PByte;
  R, G, B: Byte;
  YVal, CbVal, CrVal: Byte;
  W, H: Integer;
begin
  if (Source = nil) or Source.Empty then
    Exit;

  // È·±£Î»Í¼¸ñÊ½Îª 24 Î»
  if Source.PixelFormat <> pf24bit then
    Source.PixelFormat := pf24bit;

  W := Source.Width;
  H := Source.Height;

  // µ÷Õû¾ØÕó´óĞ¡
  YPlane.RowCount := H;
  YPlane.ColCount := W;
  CbPlane.RowCount := H;
  CbPlane.ColCount := W;
  CrPlane.RowCount := H;
  CrPlane.ColCount := W;

  // ÖğÏñËØ×ª»»
  for Y := 0 to H - 1 do
  begin
    P := Source.ScanLine[Y];
    for X := 0 to W - 1 do
    begin
      // ¶ÁÈ¡ BGR Ë³Ğò£¨Windows Î»Í¼¸ñÊ½£©
      B := P^;
      Inc(P);
      G := P^;
      Inc(P);
      R := P^;
      Inc(P);

      // ×ª»»µ½ YCbCr
      RGBToYCbCr(R, G, B, YVal, CbVal, CrVal);

      // ´æ´¢µ½¾ØÕó£¨×¢Òâ£º¾ØÕóË÷ÒıÊÇ [Col, Row]£©
      YPlane[X, Y] := YVal;
      CbPlane[X, Y] := CbVal;
      CrPlane[X, Y] := CrVal;
    end;
  end;
end;

procedure TCnColorSpaceConverter.ConvertYCbCrToBitmap(YPlane, CbPlane,
  CrPlane: TCnFloatMatrix; Dest: TBitmap);
var
  X, Y: Integer;
  P: PByte;
  R, G, B: Byte;
  YVal, CbVal, CrVal: Byte;
  W, H: Integer;
begin
  if (YPlane = nil) or (CbPlane = nil) or (CrPlane = nil) or (Dest = nil) then
    Exit;

  // ¼ì²é¾ØÕó³ß´çÒ»ÖÂĞÔ
  if (YPlane.RowCount <> CbPlane.RowCount) or
     (YPlane.RowCount <> CrPlane.RowCount) or
     (YPlane.ColCount <> CbPlane.ColCount) or
     (YPlane.ColCount <> CrPlane.ColCount) then
    raise Exception.Create('YCbCr planes must have the same dimensions');

  W := YPlane.ColCount;
  H := YPlane.RowCount;

  // ÉèÖÃÎ»Í¼´óĞ¡ºÍ¸ñÊ½
  Dest.PixelFormat := pf24bit;
  Dest.Width := W;
  Dest.Height := H;

  // ÖğÏñËØ×ª»»
  for Y := 0 to H - 1 do
  begin
    P := Dest.ScanLine[Y];
    for X := 0 to W - 1 do
    begin
      // ´Ó¾ØÕó¶ÁÈ¡ YCbCr Öµ
      YVal := ClipByte(YPlane[X, Y]);
      CbVal := ClipByte(CbPlane[X, Y]);
      CrVal := ClipByte(CrPlane[X, Y]);

      // ×ª»»µ½ RGB
      YCbCrToRGB(YVal, CbVal, CrVal, R, G, B);

      // Ğ´Èë BGR Ë³Ğò£¨Windows Î»Í¼¸ñÊ½£©
      P^ := B;
      Inc(P);
      P^ := G;
      Inc(P);
      P^ := R;
      Inc(P);
    end;
  end;
end;

procedure TCnColorSpaceConverter.ExtractYChannel(Source: TBitmap;
  YPlane: TCnFloatMatrix);
var
  X, Y: Integer;
  P: PByte;
  R, G, B: Byte;
  YVal, CbVal, CrVal: Byte;
  W, H: Integer;
begin
  if (Source = nil) or Source.Empty then
    Exit;

  // È·±£Î»Í¼¸ñÊ½Îª 24 Î»
  if Source.PixelFormat <> pf24bit then
    Source.PixelFormat := pf24bit;

  W := Source.Width;
  H := Source.Height;

  // µ÷Õû¾ØÕó´óĞ¡
  YPlane.RowCount := H;
  YPlane.ColCount := W;

  // ÖğÏñËØÌáÈ¡ Y Í¨µÀ
  for Y := 0 to H - 1 do
  begin
    P := Source.ScanLine[Y];
    for X := 0 to W - 1 do
    begin
      // ¶ÁÈ¡ BGR Ë³Ğò
      B := P^;
      Inc(P);
      G := P^;
      Inc(P);
      R := P^;
      Inc(P);

      // Ö»¼ÆËã Y ·ÖÁ¿£¨²»ĞèÒª Cb ºÍ Cr£©
      RGBToYCbCr(R, G, B, YVal, CbVal, CrVal);

      // ´æ´¢ Y Öµµ½¾ØÕó
      YPlane[X, Y] := YVal;
    end;
  end;
end;

initialization
  BitmapList := TThreadList.Create;
  BitmapList.duplicates := dupIgnore;   // ÖØ¸´¼ÓÈëÊ±ºöÂÔ
  CnCanvasList := TThreadList.Create;
  CnCanvasList.duplicates := dupIgnore; // ÖØ¸´¼ÓÈëÊ±ºöÂÔ
  GdiActTimer := TTimer.Create(nil);
  GdiActTimer.Interval := FreeGdiInterval;
  GdiActTimer.OnTimer := TCnBitmap.OnGdiActTimer;
  GdiActTimer.Enabled := True;
  InitGrayPal;

finalization
  GdiActTimer.Free;
  BitmapList.Free;
  CnCanvasList.Free;

end.

