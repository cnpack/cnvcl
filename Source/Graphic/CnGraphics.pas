{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2025 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��https://www.cnpack.org                                  }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnGraphics;
{* |<PRE>
================================================================================
* ������ƣ�����ؼ���
* ��Ԫ���ƣ�����ؼ���ԭ����ͼ����Ԫ
* ��Ԫ���ߣ��ܾ��� (zjy@cnpack.org)
* ��    ע���õ�ԪΪ�ɵ�ͼ��⣬��ͼ�����������������
* ����ƽ̨��PWin98SE + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2022.09.25 V0.12
*               ���� Win64 λ��֧��
*           2002.03.14 V0.11Alpha
*               ԭͼ��������汾
*               �¿�����������
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, SysUtils, Classes, Graphics, ExtCtrls, Math, Controls, Messages,
  CnNative, CnClasses, CnCommon, CnGraphConsts;

type

//--------------------------------------------------------//
// ͼ����������Ͷ���                                   //
//--------------------------------------------------------//

  PCnColor = ^TCnColor;
  {* ָ��TCnColor��ָ������}
  TCnColor = packed record
  {* 24λ��ɫֵ��¼}
    b, g, r: Byte;
  end;

  PCnLine = ^TCnLine;
  {* ָ�� TCnLine ��ָ�����ͣ�һ��ָ��һ��λͼɨ��������}
  TCnLine = array[0..65535] of TCnColor;
  {* TCnColor �������ͣ�һ��Ϊһ��λͼɨ��������}
  PPCnLines = ^TPCnLines;
  {* ָ�� TPCnLines ��ָ�����ͣ�һ������ָ��λͼɨ���ߵ�ַ����}
  TPCnLines = array[0..65535] of PCnLine;
  {* PCnLine �������ͣ�һ�����ڴ洢λͼɨ���ߵ�ַ����}

  TFilterCore = array[0..2, 0..2] of SmallInt;
  {* ���� 3x3 ͼ��������ľ���˳���}

  TPointF = record
  {* �����������¼����}
    x, y: Single;
  end;
  TPointFArray = array of TPointF;
  {* TPointF ���������궯̬���飬һ������ͼ�λ��Ʋ�������}

  TRectF = record
  {* ���������μ�¼����}
    case Integer of
      0: (Left, Top, Right, Bottom: Single);
      1: (TopLeft, BottomRight: TPointF);
  end;

  TCnDrawMode = (dmDraw, dmCenter, dmStretched, dmResize, dmTiled);
  {* ͼ�����ģʽ
   |<BR>
   |<BR>    dmDraw       ��Դͼ��ֱ�ӻ�����Ŀ��ͼ�����Ͻ�
   |<BR>    dmCenter     ��Դͼ����Ƶ�Ŀ��ͼ������λ��
   |<BR>    dmStretched  ��Դͼ�����Ż��Ƶ�Ŀ��ͼ��
   |<BR>    dmResize     ���Ż���ʱ����Դͼ�񳤿��
   |<BR>    dmTiled      Դͼ��ƽ�̻��Ƶ�Ŀ��ͼ��
  }

const
  csMaxAlpha = High(Byte);
  csMaxProgress = 100;

type
  TCnAlpha = Byte;
  {* ͼ�� Alpha ���ϵ�� 0..255��0 ��ʾȫ͸����255 ��ʾ��͸��}
  TCnProgress = 0..csMaxProgress;
  {* ������̰ٷֱ����� 0..100}

//--------------------------------------------------------//
// ͼ�����쳣���Ͷ���                                   //
//--------------------------------------------------------//

type
  ECnGraphics = class(Exception);
  {* CnPack ����ͼ����쳣����}

  EInvalidPixel = class(ECnGraphics);
  {* ��Ч�����ص��쳣��ͨ������Ϊ���� TCnBitmap �����ص�ʱ��Χ����}
  EInvalidScanLine = class(ECnGraphics);
  {* ��Ч��ɨ�����쳣��ͨ������Ϊ���� TCnBitmap ��ɨ����ʱ��Χ����}
  EBitmapIsEmpty = class(ECnGraphics);
  {* �޷����ʿ�λͼ�쳣��ͨ������Ϊ����û��λͼ���ݵ� TCnBitmap �����ػ�ɨ����}
  EInvalidForeBmp = class(ECnGraphics);
  {* ��Ч��ǰ��ͼ�쳣���ڻ���ƽ������ʱ�������ڲ�������}

//--------------------------------------------------------//
// ������ɫ��                                             //
//--------------------------------------------------------//

const
  csMaxGradPos = 100;

type
  TCnGradPos = 0..csMaxGradPos;
  {* ������ɫλ������ 0..100�����ڱ�ʶһ���м���ɫ�ڽ���ɫ���е�λ��}
  TCnGradStyle = (gsLeftToRight, gsRightToLeft, gsTopToBottom, gsBottomToTop,
    gsCenterToLR, gsCenterToTB, gsRadial);
  {* ����ɫ����ģʽ
   |<BR>
   |<BR>    gsLeftToRight     �����ҽ���
   |<BR>    gsRightToLeft     ���ҵ��󽥱�
   |<BR>    gsTopToBottom     ���ϵ��½���
   |<BR>    gsBottomToTop     ���µ��Ͻ���
   |<BR>    gsCenterToLR      ���м䵽���߽���
   |<BR>    gsCenterToTB      ���м䵽���½���
   |<BR>    gsRadial          ���м����ܷ��佥��
  }
  TCnMiddleColor = class;

{ TCnMiddleColorItem }

  TCnMiddleColorItem = class(TCollectionItem)
  {* ������ɫ�����м�ɫ������}
  private
    FColor: TColor;
    FPos: TCnGradPos;
    procedure SetColor(const Value: TColor);
    procedure SetPos(const Value: TCnGradPos);
  public
    constructor Create(Collection: TCollection); override;
    {* ��������Ӧ����һ�� TCnMiddleColor ������������һ�㲻��Ҫ�ֶ�����}
    procedure Assign(Source: TPersistent); override;
    {* ��ֵ������һ�㲻��Ҫ�ֶ�����}
  published
    property Color: TColor read FColor write SetColor;
    {* ������ɫ�����м���ɫֵ}
    property Pos: TCnGradPos read FPos write SetPos;
    {* ������ɫ�����м���ɫ��ɫ���е�λ��}
  end;

{ TCnMiddleColor }

  TCnMiddleColor = class(TOwnedCollection)
  {* ������ɫ�����м�ɫ�ռ����࣬��Ҫ���� TCnGradientColor ������}
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
    {* �����������ڴ���һ�������ʵ��}
    procedure Add(AColor: TColor; APos: TCnGradPos);
    {* ������������������м�ɫ��������һ����ɫ��
     |<BR>
     |<BR>    AColor: TColor      ���ӵ���ɫֵ
     |<BR>    APos: TCnGradPos    ����ɫ�ڽ���ɫ���е�λ��
    }
    procedure Sort;
    {* �Խ���ɫ��λ������һ�㲻��Ҫ�ֶ�����}
    property Items[Index: Integer]: TCnMiddleColorItem read GetItem write SetItem;
    default;
    {* ����ɫ����ɫ������������ʽ����ɫ���е�����}
  end;

{ TCnGradientColor }

  TCnGradientColor = class(TCnPersistent)
  {* ������ɫ�࣬�����˽���ɫ���Ʋ���������Ϊ�������ݸ� TCnBitmap �Ľ���ɫ���Ʒ�ʽ}
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
    {* �����������ڴ���һ�������ʵ��}
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    {* ��ֵ������һ�㲻��Ҫ�ֶ�����}
  published
    property ColorMiddle: TCnMiddleColor read GetColorMiddle write SetColorMiddle;
    {* ������ɫ���м�ɫ����}
    property ColorStart: TColor read FColorStart write SetColorStart default clBlack;
    {* ������ʼɫ}
    property ColorEnd: TColor read FColorEnd write SetColorEnd default clBlack;
    {* �������ɫ}
    property Style: TCnGradStyle read FStyle write SetStyle default gsLeftToRight;
    {* ���䷽ʽ}
  end;

//--------------------------------------------------------//
// ƽ����Ч������                                         //
//--------------------------------------------------------//

  TFontQuality = (fqHigh, fqNormal, fqLow, fqNone);
  {* ƽ��������ƾ�������
   |<BR>
   |<BR>    fqHigh      �߾��Ȼ��ƣ�����4x4�������ٶȽ���
   |<BR>    fqNormal    ��ͨ���Ȼ��ƣ�����3x3����������ٶ������ȣ�Ĭ��ֵ
   |<BR>    fqLow       �;��Ȼ��ƣ�����2x2�������ٶȽϿ�
   |<BR>    fqNone      ��ƽ��Ч��
  }
  TFontStyleEx = (fsShadow, fsGradient, fsTexture, fsNoise, fsOutline,
    fsLighting, fsSpray);
  {* ƽ��������չ��Ч���
   |<BR>
   |<BR>    fsShadow      ������ӰЧ������ TCnShadow
   |<BR>    fsGradient    �����ı���ɫ����Ч������ TCnGradientColor
   |<BR>    fsTexture     �����ı�����ͼ
   |<BR>    fsNoise       �����ı���������
   |<BR>    fsOutline     �����ı��������߷�ʽ��ʾ
   |<BR>    fsLighting    ���õƹ�Ч������ TCnLighting
   |<BR>    fsSpray       �����罦Ч��
  }
  TFontStyleExs = set of TFontStyleEx;
  {* ƽ��������չ��Ч��񼯺�}

{ TCnShadow }

  TShadowOffset = -20..20;
  {* ��Ӱƫ�Ʒ�Χ}
  TShadowBlur = 0..10;
  {* ��Ӱģ����}

  TCnShadow = class(TCnPersistent)
  {* ��Ӱ�����࣬��������ƽ�������ͼ����Ч����Ӱ����}
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
    {* �����������ڴ���һ�������ʵ��}
    procedure Assign(Source: TPersistent); override;
    {* ��ֵ������һ�㲻��Ҫ�ֶ�����}
  published
    property Blur: TShadowBlur read FBlur write SetBlur default 1;
    {* ��Ӱģ�����ԣ�����3X3��˹ģ���㷨������Ӱģ������}
    property Alpha: TCnAlpha read FAlpha write SetAlpha default 180;
    {* ��Ӱ�Ĳ�͸��������}
    property Color: TColor read FColor write SetColor default $00444444;
    {* ��Ӱ��ɫ}
    property OffsetX: TShadowOffset read FOffsetX write SetOffsetX default 2;
    {* ��Ӱ��ˮƽ�����ƫ��������ΧΪ -20..20��Ϊ����ʾ����ƫ}
    property OffsetY: TShadowOffset read FOffsetY write SetOffsetY default 2;
    {* ��Ӱ�ڴ�ֱ�����ƫ��������ΧΪ -20..20��Ϊ����ʾ����ƫ}
  end;

{ TCnShadow }

  TLightingOffset = -200..200;
  {* �������ĵ�ƫ�Ʒ�Χ���ٷֱȣ�Ϊ����ʾ��ƫ}
  TLightingRange = 0..1000;
  {* ���շ�Χ���ٷֱ�}

  TCnLighting = class(TCnPersistent)
  {* ����Ч�������࣬��������ƽ�������ͼ����Ч�ĵƹ�Ч������}
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
    {* �����������ڴ���һ�������ʵ��}
    procedure Assign(Source: TPersistent); override;
    {* ��ֵ������һ�㲻��Ҫ�ֶ�����}
  published
    property Alpha: TCnAlpha read FAlpha write SetAlpha default 180;
    {* ����Ч���Ĳ�͸��������}
    property Color: TColor read FColor write SetColor default clWhite;
    {* �ƹ���ɫ}
    property OffsetX: TLightingOffset read FOffsetX write SetOffsetX default 0;
    {* �������ĵ�ƫ�Ʒ�Χ��Ŀ����ο�ȵİٷֱȣ���Ϊ����ʾ��ƫ����ΧΪ -200..200}
    property OffsetY: TLightingOffset read FOffsetY write SetOffsetY default 0;
    {* �������ĵ�ƫ�Ʒ�Χ��Ŀ����θ߶ȵİٷֱȣ���Ϊ����ʾ��ƫ����ΧΪ -200..200}
    property Width: TLightingRange read FWidth write SetWidth default 80;
    {* ���շ�Χ��ȣ�Ŀ����ο�ȵİٷֱȣ�����ΧΪ 0..1000}
    property Height: TLightingRange read FHeight write SetHeight default 80;
    {* ���շ�Χ�߶ȣ�Ŀ����ο�ȵİٷֱȣ�����ΧΪ 0..1000}
    property Angle: Double read FAngle write SetAngle;
    {* ���շ�Χ�Ƕȣ���ΧΪ -360..360}
  end;

{ TCnFont }

  TCnFont = class(TFont)
  {* ƽ����Ч�����࣬�� TFont �����������ṩ��һЩ��Ч��ʾ����}
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
    {* �����������ڴ���һ�������ʵ��}
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    {* ��ֵ�����������TFont�и�ֵ}
  published
    property StyleEx: TFontStyleExs read FStyleEx write SetStyleEx;
    {* ��չ��������Ч��ʾ������TFontStyleExs�������ͣ�ͬʱ��������һЩ�����Ƿ���}
    property Quality: TFontQuality read FQuality write SetQuality default fqNormal;
    {* ����ƽ����ʾ����}
    property Shadow: TCnShadow read FShadow write SetShadow;
    {* ������Ӱ��ʾ�������� StyleEx ����Ӱ��}
    property Gradient: TCnGradientColor read FGradient write SetGradient;
    {* ����ǰ������Ч���������� StyleEx ����Ӱ��}
    property Lighting: TCnLighting read FLighting write SetLighting;
    {* ����ǰ������Ч���������� StyleEx ����Ӱ��}
    property Texture: TPicture read GetTexture write SetTexture;
    {* ����ǰ������Ч���������� StyleEx ����Ӱ��}
    property TextureMode: TCnDrawMode read FTextureMode write SetTextureMode
      default dmTiled;
    {* ����ǰ������Ч����ʾģʽ}
    property Alpha: TCnAlpha read FAlpha write SetAlpha default csMaxAlpha;
    {* ���岻͸���Ȳ���}
    property Noise: Byte read FNoise write SetNoise default 0;
    {* ����ǰ���������Ч���������� StyleEx ����Ӱ��}
    property Spray: Byte read FSpray write SetSpray default 0;
    {* ����ǰ���罦Ч���������� StyleEx ����Ӱ��}
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
// ����ͼ������                                         //
//--------------------------------------------------------//

  TGdiAllocStyle = (gsInternal, gsNormal, gsActive);
  {* GDI ��Դ����ʽ
   |<BR>
   |<BR>                  DC        HBITMAP
   |<BR>    gsInternal:  ��ʱ�ͷ�  ��ʱ�ͷ�
   |<BR>    gsNormal:    ��ʱ�ͷ�  ��ʱ�ͷ�
   |<BR>    gsActive:    ��ʱ�ͷ�  �Ӳ��ͷ�
  }

  TAdjustRange = -100..100;
  {* ͼ�����Ա仯��Χ}

  TPenWeight = (pwThin, pwNormal, pwThick);
  {* ͼ�񿹾�ݻ��ʴ�ϸ�̶ȣ���Ϊ1�����ؿ������
   |<BR>
   |<BR>    pwThin:    ϸ���ʣ����Ƴ���ͼ�ν�ǳ
   |<BR>    pwNormal:  ���滭�ʣ����Ƴ���ͼ������
   |<BR>    pwThick:    �ֻ��ʣ����Ƴ���ͼ�νϴ�
  }

  TColorChannel = (ccRed, ccGreen, ccBlue);
  {* ͼ����ɫͨ��
   |<BR>
   |<BR>    ccRed:    ��ɫͨ��
   |<BR>    ccGreen:  ��ɫͨ��
   |<BR>    ccBlue:    ��ɫͨ��
  }
  TColorChannels = set of TColorChannel;
  {* ͼ����ɫͨ������}

  TTurnAngle = (ta90, ta180, ta270);

const
  csAllChannels = [ccRed, ccGreen, ccBlue];

type
  TCnBitmap = class;

{ TCnCanvas }

  TCnCanvas = class(TCanvas)
  {* ���� TCnBitmap �ڲ��Ļ����࣬����ֱ��ʹ��}
  private
    FBitmap: TCnBitmap;
    FDC: HDC;
    procedure FreeContext;
    function GetHandle: HDC;
  protected
    procedure CreateHandle; override;
  public
    constructor Create(ABitmap: TCnBitmap);
    {* �����������ڴ���һ�������ʵ��}
    destructor Destroy; override;
    property Handle: HDC read GetHandle;
    {* ���������������Ϊֻ������}
  end;

{ TCnBitmap }

  TCnBitmap = class(TCnPersistent)
  {* CnPack ����ͼ����}
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
    {* �����������ڴ���һ�������ʵ��}
    destructor Destroy; override;

    // ��������
    procedure Fill(Color: TColor = clBlack);
    {* ��ָ����ɫ�������ͼ��Ĭ��Ϊ��ɫ}
    procedure FillRect(Rect: TRect; Color: TColor = clBlack);
    {* ��ָ����ɫ�������һ��ͼ������}
    procedure DrawLine(x1, y1, x2, y2: Integer; Color: TColor);
    {* ��ָ����ɫ����һ��ֱ��
     |<BR>
     |<BR> x1, y1: Integer    ��ʼ������
     |<BR> x2, y2: Integer    ����������}
    procedure FrameRect(Rect: TRect; Color: TColor);
    {* ��ָ����ɫ����һ�����ο��}
    procedure FreeImage; virtual;
    {* ������λͼ��գ��ͷ������ѷ������Դ}
    function CreateRegion(var RgnData: PRgnData): Integer;
    {* ���ݵ�ǰ��͸��ɫ���� TransparentColor ��λͼ�д���һ�� Region �������ݡ�
     |<BR> �������� RgnData Ϊ PRgnData ָ�����ͣ����ڽ������ݽ����
     |<BR> ����ֵΪ������ݿ���ֽ�����}

    // ���ⲿ��������
    procedure Assign(Source: TPersistent); override;
    {* ��ֵ���̣����������ͼ������л�ȡ���ݡ�
     |<BR>
     |<BR> ֧�����µ�Դͼ�����ͣ�
     |<BR> TCnBitmap��TBitmap��TGraphic��TPicture �Լ����ǵ��������� TIcon��TJpegImage ��
     |<BR> ��SourceΪnilʱ����յ�ǰλͼ���ͷ������ѷ������Դ��}
    procedure SetSize(AWidth, AHeight: Integer); overload;
    {* ���õ�ǰͼ���С����������С��ԭͼ�����ݽ���ʧ��}
    procedure SetSize(ARect: TRect); overload;
    {* ���õ�ǰͼ���С����������С��ԭͼ�����ݽ���ʧ}
    procedure LoadBlank(AWidth, AHeight: Integer);
    {* ���õ�ǰͼ���С������ͬSetSize��}
    procedure LoadFromMemory(ABits: Pointer; AWidth, AHeight: Integer);
    {* ���ڴ���װ��λͼ
     |<BR>
     |<BR> ABits: Pointer    ָ��һ������������������Ӧ���� 24 λ��ʽ��ͼ������
     |<BR> AWidth, AHeight: Integer    λͼ�Ŀ�Ⱥ͸߶�}
    procedure LoadFromStream(Stream: TStream);
    {* ������װ��λͼ}
    procedure LoadFromFile(const FileName: string);
    {* ��ͼ���ļ���װ��λͼ
     |<BR> �ڲ�ʹ��TPicture����ȡ�ļ�������ϵͳ��֧�ֵ�ͼ���ļ���ʽ��
     |<BR> �� Icon��Wmf��Jpeg����Ҫ Jpeg ��Ԫ����ͼ���ļ�}
    procedure LoadFromResourceName(instance: THandle; const ResName: string);
    {* ����Դ��װ��λͼ������Ϊģ������ BITMAP ��Դ��}
    procedure LoadFromResourceID(instance: THandle; ResID: Integer);
    {* ����Դ��װ��λͼ������Ϊģ������ BITMAP ��Դ ID}
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
      APalette: HPALETTE);
    {* �Ӽ�������װ��λͼ}
    procedure SaveToStream(Stream: TStream);
    {* ����ǰλͼ���浽��}
    procedure SaveToFile(const FileName: string);
    {* ����ǰλͼ���浽 Bmp �ļ����ļ���ʽΪ 24 λ Bmp λͼ�ļ�}
    procedure SaveToClipboardFormat(var Format: Word; var Data: THandle;
      var APalette: HPALETTE);
    {* ����λͼ����������}

    // ͼ����Ʒ���
    procedure Draw(DstX, DstY: Integer; Src: TCnBitmap); overload;
    {* ͼ����Ʒ�������Դͼ��ȫ�����Ƶ���ǰλͼ��
     |<BR>
     |<BR> DstX, DstY: Integer    ��ǰͼ������Ͻ�����
     |<BR> Src: TCnBitmap    Դͼ��}
    procedure DrawEx(DstX, DstY: Integer; Src: TCnBitmap; SrcRect: TRect); overload;
    {* ��ǿ��ͼ����Ʒ�������Դͼ���е�һ���ֻ��Ƶ���ǰλͼ��
     |<BR>
     |<BR> DstX, DstY: Integer    ��ǰͼ������Ͻ�����
     |<BR> Src: TCnBitmap    Դͼ��
     |<BR> SrcRect: TRect    Դͼ�����}
    procedure Draw(DstX, DstY: Integer; Src: TGraphic); overload;
    {* ͼ����Ʒ�������Դͼ��ȫ�����Ƶ���ǰλͼ��
     |<BR>
     |<BR> DstX, DstY: Integer    ��ǰͼ������Ͻ�����
     |<BR> Src: TGraphic    Դͼ�񣬿�����TIcon��TBitmap��TJpegImage��������}
    procedure DrawEx(DstX, DstY: Integer; Src: TGraphic; SrcRect: TRect); overload;
    {* ��ǿ��ͼ����Ʒ�������Դͼ���е�һ���ֻ��Ƶ���ǰλͼ��
     |<BR>
     |<BR> DstX, DstY: Integer    ��ǰͼ������Ͻ�����
     |<BR> Src: TGraphic     Դͼ�񣬿����� TIcon��TBitmap��TJpegImage ��������
     |<BR> SrcRect: TRect    Դͼ�����}
    procedure Draw(DstX, DstY: Integer; hSrc: HDC; SrcRect: TRect); overload;
    {* ͼ����Ʒ�������ԴDC�ϵ�һ���ֻ��Ƶ���ǰλͼ��
     |<BR>
     |<BR> DstX, DstY: Integer    Ϊ��ǰͼ������Ͻ�����
     |<BR> hSrc: HDC         Դ DC ����������� TCanvas.Handle
     |<BR> SrcRect: TRect    Դͼ�����}
    procedure DrawTo(hDst: HDC; DstX, DstY: Integer); overload;
    {* ͼ����Ʒ���������ǰλͼȫ�����Ƶ�Ŀ�� DC ��
     |<BR>
     |<BR> hDst: HDC        Ŀ�� DC ����������� TCanvas.Handle
     |<BR> DstX, DstY: Integer    Ŀ�껭�������Ͻ�����}
    procedure DrawToEx(hDst: HDC; DstX, DstY: Integer; SrcRect: TRect); overload;
    {* ��ǿ��ͼ����Ʒ���������ǰ��һ���ֻ��Ƶ�Ŀ�� DC ��
     |<BR>
     |<BR> hDst: HDC        Ŀ�� DC ����������� TCanvas.Handle
     |<BR> DstX, DstY: Integer    Ŀ�껭�������Ͻ�����
     |<BR> SrcRect: TRect    ��ǰͼ��Դ����}
    procedure DrawMode(Src: TCnBitmap; Mode: TCnDrawMode); overload;
    {* ָ��ģʽ��ͼ����Ʒ�������Դͼ��ָ����ģʽ���Ƶ���ǰͼ����
     |<BR>
     |<BR> Src: TCnBitmap     Դͼ��
     |<BR> Mode: TCnDrawMode  ���Ʒ�ʽ}
    procedure DrawModeEx(Src: TCnBitmap; Mode: TCnDrawMode; Alpha: TCnAlpha); overload;
    {* ָ��ģʽ��ͼ����Ʒ�������Դͼ��ָ����ģʽ���Ƶ���ǰͼ���ϣ�֧�� Alpha ���
     |<BR>
     |<BR> Src: TCnBitmap     Դͼ��
     |<BR> Mode: TCnDrawMode  ���Ʒ�ʽ}
    procedure DrawMode(Src: TGraphic; Mode: TCnDrawMode); overload;
    {* ָ��ģʽ��ͼ����Ʒ�������Դͼ��ָ����ģʽ���Ƶ���ǰͼ����
     |<BR>
     |<BR> Src: TGraphic      Դͼ�񣬿�����TIcon��TBitmap��TJpegImage��������
     |<BR> Mode: TCnDrawMode  ���Ʒ�ʽ}
    procedure DrawModeEx(Src: TGraphic; Mode: TCnDrawMode; Alpha: TCnAlpha); overload;
    {* ָ��ģʽ��ͼ����Ʒ�������Դͼ��ָ����ģʽ���Ƶ���ǰͼ���ϣ�֧�� Alpha ���
     |<BR>
     |<BR> Src: TGraphic      Դͼ�񣬿�����TIcon��TBitmap��TJpegImage��������
     |<BR> Mode: TCnDrawMode  ���Ʒ�ʽ}

    // ���Ļ���
    procedure CenterDraw(Src: TCnBitmap); overload;
    {* ��Դͼ����Ƶ���ǰͼ�������λ����
     |<BR>
     |<BR> Src: TCnBitmap      Դͼ��}
    procedure CenterDraw(Src: TGraphic); overload;
    {* ��Դͼ����Ƶ���ǰͼ�������λ����
     |<BR>
     |<BR> Src: TGraphic      Դͼ�񣬿����� TIcon��TBitmap��TJpegImage ��������}

    // ƽ�̻���
    procedure TileDraw(Src: TCnBitmap); overload;
    {* ��Դͼ��ƽ�̻��Ƶ���ǰͼ��
     |<BR>
     |<BR> Src: TCnBitmap      Դͼ��}
    procedure TileDrawEx(DstRect: TRect; Src: TCnBitmap); overload;
    {* ��Դͼ��ƽ�̻��Ƶ���ǰͼ��ָ��������
     |<BR>
     |<BR> DstRect: TRect      ��ǰͼ��Ŀ�����
     |<BR> Src: TCnBitmap      Դͼ��}
    procedure TileDraw(Src: TGraphic); overload;
    {* ��Դͼ��ƽ�̻��Ƶ���ǰͼ��
     |<BR>
     |<BR> Src: TGraphic      Դͼ�񣬿����� TIcon��TBitmap��TJpegImage ��������}
    procedure TileDrawEx(DstRect: TRect; Src: TGraphic); overload;
    {* ��Դͼ��ƽ�̻��Ƶ���ǰͼ��ָ��������
     |<BR>
     |<BR> DstRect: TRect      ��ǰͼ��Ŀ�����
     |<BR> Src: TCnBitmap      Դͼ�񣬿����� TIcon��TBitmap��TJpegImage ��������}
    procedure TileDrawTo(hDst: HDC; DstRect: TRect); overload;
    {* ����ǰͼ��ƽ�̻��Ƶ�Ŀ��DCָ��������
     |<BR>
     |<BR> hDst: HDC          Ŀ�� DC �����������TCanvas.Handle
     |<BR> DstRect: TRect     Ŀ�����}

    // ���Ż���
    procedure StretchDraw(Src: TCnBitmap); overload;
    {* ��Դͼ�����Ż��Ƶ���ǰͼ����
     |<BR>
     |<BR> Src: TCnBitmap    Դͼ��}
    procedure StretchDrawEx(DstRect, SrcRect: TRect; Src: TCnBitmap); overload;
    {* ��Դͼ���һ�������Ż��Ƶ���ǰͼ����ָ��������
     |<BR>
     |<BR> DstRect: TRect    Ŀ�����
     |<BR> SrcRect: TRect    Դ����
     |<BR> Src: TCnBitmap    Դͼ��}
    procedure StretchDraw(Src: TGraphic); overload;
    {* ��Դͼ�����Ż��Ƶ���ǰͼ����
     |<BR>
     |<BR> Src: TGraphic     Դͼ�񣬿����� TIcon��TBitmap��TJpegImage ��������}
    procedure StretchDrawEx(DstRect, SrcRect: TRect; Src: TGraphic); overload;
    {* ��Դͼ���һ�������Ż��Ƶ���ǰͼ��ָ��������
     |<BR>
     |<BR> DstRect: TRect     Ŀ�����
     |<BR> SrcRect: TRect     Դ����
     |<BR> Src: TGraphic      Դͼ�񣬿����� TIcon��TBitmap��TJpegImage ��������}
    procedure StretchDraw(SrcRect: TRect; hSrc: HDC); overload;
    {* ��ԴDC�ϵ�ָ���������Ż��Ƶ���ǰͼ����
     |<BR>
     |<BR> SrcRect: TRect     Դ����
     |<BR> hSrc: HDC          Դ DC ����������� TCanvas.Handle}
    procedure StretchDrawEx(DstRect, SrcRect: TRect; hSrc: HDC); overload;
    {* ��ԴDC�ϵ�ָ���������Ż��Ƶ���ǰͼ��ָ��������
     |<BR>
     |<BR> DstRect: TRect     Ŀ�����
     |<BR> SrcRect: TRect     Դ����
     |<BR> hSrc: HDC          Դ DC ����������� TCanvas.Handle}
    procedure StretchDrawTo(Dst: TImage); overload;
    {* ����ǰͼ�����Ż��Ƶ�TImage�ؼ���
     |<BR>
     |<BR> Dst: TImage       Ŀ��ؼ�}
    procedure StretchDrawTo(hDst: HDC; DstRect: TRect); overload;
    {* ����ǰͼ�����Ż��Ƶ�DC��
     |<BR>
     |<BR> hDst: HDC         Ŀ�� DC ����������� TCanvas.Handle
     |<BR> DstRect: TRect    Ŀ�����}
    procedure StretchDrawToEx(hDst: HDC; DstRect, SrcRect: TRect); overload;
    {* ����ǰͼ���һ�������Ż��Ƶ�DC��
     |<BR>
     |<BR> hDst: HDC         Ŀ��DC����������� TCanvas.Handle
     |<BR> DstRect: TRect    Ŀ�����
     |<BR> SrcRect: TRect    Դ����}

    // Alpha��ϻ���
    procedure AlphaDraw(Src: TCnBitmap; Alpha: TCnAlpha; Stretch: Boolean); overload;
    {* ��Դͼ���뵱ǰͼ��ָ���ı�����ϵ���ǰͼ����
     |<BR>
     |<BR> Src: TCnBitmap     Դͼ��
     |<BR> Alpha: TCnAlpha    Դͼ��Ĳ�͸����
     |<BR> Stretch: Boolean   ��ͼ���С��һ��ʱ���Ƿ��Զ���Դͼ���������}
    procedure AlphaDraw(DstX, DstY: Integer; Src: TCnBitmap; SrcRect: TRect;
      Alpha: TCnAlpha); overload;
    {* ��Դͼ���е�һ�����뵱ǰͼ��ָ���ı�����ϵ���ǰͼ��ָ��λ����
     |<BR>
     |<BR> DstX, DstY: Integer    Ŀ��λ�����Ͻ�����
     |<BR> Src: TCnBitmap         Դͼ��
     |<BR> SrcRect: TRect         Դ����
     |<BR> Alpha: TCnAlpha        Դͼ��Ĳ�͸����}
    procedure AlphaDrawGrad(Src: TCnBitmap; Style: TCnGradStyle;
      Stretch: Boolean; StartAlpha: TCnAlpha = 0; EndAlpha: TCnAlpha = csMaxAlpha);
    {* ��Դͼ���뵱ǰͼ�񰴽���ı�����ϵ���ǰͼ��ָ��λ����
     |<BR>
     |<BR> Src: TCnBitmap         Դͼ��
     |<BR> Style: TCnGradStyle    �����Ϸ�ʽ
     |<BR> Stretch: Boolean       ��ͼ���С��һ��ʱ���Ƿ��Զ���Դͼ���������
     |<BR> StartAlpha: TCnAlpha   ������ʼ�Ĳ�͸����
     |<BR> EndAlpha: TCnAlpha     ��������Ĳ�͸����}
    procedure AlphaDrawEx(DstRect: TRect; Front, Back: TCnBitmap; Alpha: TCnAlpha;
      Stretch: Boolean);
    {* ������ͼ��ָ���ı�����ϵ���ǰͼ��ָ��������
     |<BR>
     |<BR> Front: TCnBitmap       ǰ��ͼ��
     |<BR> Back: TCnBitmap        ����ͼ��
     |<BR> Alpha: TCnAlpha        ǰ��ͼ��Ĳ�͸����
     |<BR> Stretch: Boolean       ��ͼ���С��һ��ʱ���Ƿ��Զ���Դͼ���������}

    // ����ɫ����
    procedure DrawGradient(GradColor: TCnGradientColor);
    {* �ڵ�ǰͼ���в���������ɫЧ��
     |<BR>
     |<BR> GradColor: TCnGradientColor    ����Ч������}
    procedure DrawGradientEx(GradColor: TCnGradientColor; Rect: TRect; Alpha:
      TCnAlpha);
    {* �ڵ�ǰͼ��ָ�������в�����͸���Ľ�����ɫЧ��
     |<BR>
     |<BR> GradColor: TCnGradientColor    ����Ч������
     |<BR> Rect: TRect        ָ������
     |<BR> Alpha: TCnAlpha    ����Ч���Ĳ�͸����
     |<BR> ע����ѡ����佥�䷽ʽʱ�����ݵ�ǰ SmoothFilter ���Կ�֧�ֿ���ݴ��� }

    // ��ťλͼ����
    procedure Disabled;
    {* ����ǰͼ��ʧЧ��ť�ķ����л��ƣ����ݵ�ǰ��͸��ɫ�����ж�}
    procedure DisabledEx(OutlineColor, BackColor, HighlightColor,
      ShadowColor: TColor; DrawHighlight: Boolean);
    {* ����ǰͼ��ʧЧ��ť�ķ����л��ƣ����ݵ�ǰ��͸��ɫ�����ж�
     |<BR>
     |<BR> OutlineColor: TColor    Ŀ��ͼ��������ɫ
     |<BR> BackColor: TColor       Ŀ��ͼ�񱳾���ɫ
     |<BR> HighlightColor: TColor  Ŀ��ͼ���������ɫ
     |<BR> ShadowColor: TColor     Ŀ��ͼ����Ӱ��ɫ
     |<BR> DrawHighlight: Boolean  �Ƿ���Ƹ�����}
    procedure DrawDisabled(hDst: HDC; ARect: TRect);
    {* ����ǰͼ��ʧЧ��ť�ķ����Ƶ�Ŀ��DC�ϣ����ݵ�ǰ��͸��ɫ�����ж�
       ��ɻ��ƺ�ǰͼ�����ݲ���
     |<BR>
     |<BR> hDst: HDC         Ŀ�� DC ����������� TCanvas.Handle
     |<BR> ARect: TRect      Ŀ�����}
    procedure DrawDisabledEx(hDst: HDC; ARect: TRect; OutlineColor,
      BackColor, HighlightColor, ShadowColor: TColor; DrawHighlight: Boolean);
    {* ����ǰͼ��ʧЧ��ť�ķ����Ƶ�Ŀ�� DC �ϣ����ݵ�ǰ��͸��ɫ�����ж�
       ��ɻ��ƺ�ǰͼ�����ݲ���
     |<BR>
     |<BR> hDst: HDC               Ŀ�� DC ����������� TCanvas.Handle
     |<BR> ARect: TRect            Ŀ�����
     |<BR> OutlineColor: TColor    Ŀ��ͼ��������ɫ
     |<BR> BackColor: TColor       Ŀ��ͼ�񱳾���ɫ
     |<BR> HighlightColor: TColor  Ŀ��ͼ���������ɫ
     |<BR> ShadowColor: TColor     Ŀ��ͼ����Ӱ��ɫ
     |<BR> DrawHighlight: Boolean  �Ƿ���Ƹ�����}
    procedure Shadowed;
    {* ����ǰͼ�񰴴���Ӱ��ť�ķ����л��ƣ����ݵ�ǰ��͸��ɫ�����ж�}
    procedure ShadowedEx(OutlineColor, ShadowColor, BackColor: TColor;
      Blur: Boolean; OffsetX, OffsetY: Integer);
    {* ����ǰͼ�񰴴���Ӱ��ť�ķ����л��ƣ����ݵ�ǰ��͸��ɫ�����ж�
     |<BR>
     |<BR> OutlineColor: TColor    Ŀ��ͼ��������ɫ
     |<BR> ShadowColor: TColor     Ŀ��ͼ����Ӱ��ɫ
     |<BR> BackColor: TColor       Ŀ��ͼ�񱳾���ɫ
     |<BR> Blur: Boolean           ��Ӱ�Ƿ�ģ��
     |<BR> OffsetX: Integer        ��Ӱˮƽƫ������Ϊ����ʾ��ƫ
     |<BR> OffsetY: Integer        ��Ӱ��ֱƫ������Ϊ����ʾ��ƫ}
    procedure DrawShadowed(hDst: HDC; ARect: TRect);
    {* ����ǰͼ�񰴴���Ӱ��ť�ķ����Ƶ�Ŀ��DC�ϣ����ݵ�ǰ��͸��ɫ�����ж�
       ��ɻ��ƺ�ǰͼ�����ݲ���
     |<BR>
     |<BR> hDst: HDC         Ŀ�� DC ����������� TCanvas.Handle
     |<BR> ARect: TRect      Ŀ�����}
    procedure DrawShadowedEx(hDst: HDC; ARect: TRect; OutlineColor, ShadowColor,
      BackColor: TColor; Blur: Boolean; OffsetX, OffsetY: Integer);
    {* ����ǰͼ�񰴴���Ӱ��ť�ķ����Ƶ�Ŀ��DC�ϣ����ݵ�ǰ��͸��ɫ�����ж�
       ��ɻ��ƺ�ǰͼ�����ݲ���
     |<BR>
     |<BR> hDst: HDC               Ŀ�� DC ����������� TCanvas.Handle
     |<BR> ARect: TRect            Ŀ�����
     |<BR> OutlineColor: TColor    Ŀ��ͼ��������ɫ
     |<BR> ShadowColor: TColor     Ŀ��ͼ����Ӱ��ɫ
     |<BR> BackColor: TColor       Ŀ��ͼ�񱳾���ɫ
     |<BR> Blur: Boolean           ��Ӱ�Ƿ�ģ��
     |<BR> OffsetX: Integer        ��Ӱˮƽƫ������Ϊ����ʾ��ƫ
     |<BR> OffsetY: Integer        ��Ӱ��ֱƫ������Ϊ����ʾ��ƫ}

    // ͼ����ɫ���Ե�������
    procedure RGB(ra, ga, ba: TAdjustRange);
    {* ������ǰͼ��ĸ���ɫ����
     |<BR>
     |<BR> ra, ga, ba: TAdjustRange  �ֱ�Ϊ�졢�̡�������������Χ
     |<BR> ��ΧֵΪ -100..100��0 ��ʾ���䣬��Ϊ���ӣ���Ϊ����}
    procedure Brightness(Range: TAdjustRange; Channels: TColorChannels =
      csAllChannels);
    {* ������ǰͼ�������
     |<BR>
     |<BR> Range: TAdjustRange  ���ȷ�ΧֵΪ -100..100��0 ��ʾ���䣬��Ϊ���ӣ���Ϊ����
     |<BR> Channels: TColorChannels    ��ɫͨ������}
    procedure Contrast(Range: TAdjustRange; Channels: TColorChannels = csAllChannels);
    {* ������ǰͼ��ĶԱȶ�
     |<BR>
     |<BR> Range: TAdjustRange  �ȶԶȷ�ΧֵΪ -100..100��0 ��ʾ���䣬��Ϊ���ӣ���Ϊ����
     |<BR> Channels: TColorChannels    ��ɫͨ������}
    procedure Saturation(Range: TAdjustRange; Channels: TColorChannels =
      csAllChannels);
    {* ������ǰͼ�����ɫ���Ͷ�
     |<BR>
     |<BR> Range: TAdjustRange  ���Ͷȷ�ΧֵΪ -100..100��0 ��ʾ���䣬��Ϊ���ӣ���Ϊ����
     |<BR> Channels: TColorChannels    ��ɫͨ������}
    procedure Levels(InLow, InHigh, OutLow, OutHigh: Byte;
      Channels: TColorChannels = csAllChannels);
    {* ������ǰͼ���ɫ��
     |<BR>
     |<BR> InLow, InHigh: Byte    ����ͼ��ĻҶ�ֵ�ϡ�����
     |<BR> OutLow, OutHigh: Byte  ���ͼ��ĻҶ�ֵ�ϡ�����
     |<BR> Channels: TColorChannels    ��ɫͨ������}
    procedure Grayscale(Channels: TColorChannels = csAllChannels);
    {* ����ǰͼ��ת��Ϊ�Ҷ�ͼ
     |<BR>
     |<BR> Channels: TColorChannels    ��ɫͨ������}
    procedure Invert(Channels: TColorChannels = csAllChannels);
    {* ����ǰͼ���������ص���ɫ��ת
     |<BR>
     |<BR> Channels: TColorChannels    ��ɫͨ������}
    procedure Colorize(Color: TColor); overload;
    {* ������ǰͼ��ָ����ɫ��ɫ����֧��ͼ��͸������
     |<BR>
     |<BR> Color: TColor  ָ����ɫֵ}
    procedure Colorize(Color: TCnColor); overload;
    {* ������ǰͼ��ָ����ɫ��ɫ����֧��ͼ��͸������
     |<BR>
     |<BR> Color: TCnColor  ָ����ɫֵ}

    // ͼ�񼸺α任
    procedure Flip(Horizontal: Boolean);
    {* ����ǰͼ�񼸺�λ�÷�ת
     |<BR>
     |<BR> Horizontal: Boolean  Ϊ���ʾˮƽ��ת��Ϊ�ٱ�ʾ��ֱ��ת}
    procedure Turn(Angle: TTurnAngle);
    {* ����ǰͼ�񼸺�λ����ת
     |<BR>
     |<BR> Angle: TTurnAngle  ת���Ƕȡ���Ϊ ta90��ta180��ta270}

    procedure VShift(Amount: Integer);
    {* ����ǰͼ����д�ֱƽ��
     |<BR>
     |<BR> Amount: Integer  ƽ����������Ϊ��}
    procedure HShift(Amount: Integer);
    {* ����ǰͼ�����ˮƽƽ��
     |<BR>
     |<BR> Amount: Integer  ƽ����������Ϊ��}
    procedure Rotate(DstCenter: TPoint; Src: TCnBitmap; Angle: Double);
    {* ��Դͼ����ת����Ƶ���ǰͼ��ָ��λ���ϣ�֧��Դͼ��͸������
     |<BR>
     |<BR> DstCenter: TPoint   Ŀ�����ĵ�λ�ã������ڵ�ǰͼ����
     |<BR> Src: TCnBitmap      Դͼ��
     |<BR> Angle: Double       Դͼ����ת�Ƕȣ���λΪ����}

    // �˾�������
    procedure ApplyFilter(Core: TFilterCore; Cent: Integer = 0);
    {* �Ե�ǰͼ��ָ��ģ����о������
     |<BR>
     |<BR> Core: TFilterCore  3x3 �����
     |<BR> Cent: Integer       ����������}
    procedure Blur;
    {* �Ե�ǰͼ�����ģ������ʹ�� 3x3 ��ֵ����}
    procedure GaussianBlur(Amount: Integer);
    {* �Ե�ǰͼ����п��ٸ�˹ģ������
     |<BR>
     |<BR> Amount: Integer    ģ���뾶}
    procedure Sharpen;
    {* �Ե�ǰͼ������񻯴���}
    procedure SharpenMore(Amount: Integer);
    {* �Ե�ǰͼ����и�����񻯴���
     |<BR>
     |<BR> Amount: Integer    �񻯳̶�}
    procedure Spray(Amount: Integer);
    {* �Ե�ǰͼ������罦�˾�����}
    procedure Emboss;
    {* ����ǰͼ�񸡵�}
    procedure Posterize(Amount: Integer);
    {* �����ڻ�Ч��}
    procedure HeightMap(Amount: Integer);
    {* ������ͼЧ��}
    procedure Marble(Scale: Double; Turbulence: Integer);
    {* ����ˮ��Ч��}
    procedure Wave(XDiv, YDiv, RatioVal: Double; Wrap: Boolean);
    {* �Ե�ǰͼ�����Ť������
     |<BR>
     |<BR> XDiv, YDiv: Double    ˮƽ����ֱ�����Ť��ϵ��
     |<BR> RatioVal: Double      Ť���̶�
     |<BR> Wrap: Boolean         ָ���Ƿ��Զ�����}
    procedure Mosaic(xAmount, yAmount: Integer);
    {* ����ǰͼ�������˻�
     |<BR>
     |<BR> xAmount: Integer    ���ο���
     |<BR> yAmount: Integer    ���ο�߶�}
    procedure Twist(Amount: Integer);
    {* ����ǰͼ��ת��Ϊ����ͼ
     |<BR>
     |<BR> Amount: Integer     �뾶ϵ��}
    procedure Lighting(Center: TPoint; OffX, OffY: Integer; Angle: Double;
      Color: TColor; Amount: TCnAlpha); overload;
    {* �ڵ�ǰͼ���ϲ�������Ч��
     |<BR>
     |<BR> Center: TPoint       �������ĵ�
     |<BR> OffX, OffY: Integer  ���շ�Χ����������뾶
     |<BR> Angle: Double        �Ƕȣ�OffX ָ���ĳ�����ˮƽ��ļн�
     |<BR> Color: TColor        ������ɫ
     |<BR> Amount: TCnAlpha     ����ǿ��}
    procedure Lighting(Rect: TRect; Data: TCnLighting); overload;
    {* �ڵ�ǰͼ���ϲ�������Ч��
     |<BR>
     |<BR> Rect: TRect        ����Ŀ�귶Χ��δ��תǰ��
     |<BR> Data: TCnLighting  ���ղ���}
    procedure Mask(MaskColor: TCnColor); overload;
    {* ����ǰͼ��ָ����ɫΪ��׼��ֵ��
     |<BR>
     |<BR> MaskColor: TCnColor    ָ������ɫ�������ɫ��ͬ�ı�Ϊ��ɫ����֮Ϊ��ɫ}
    procedure MaskEx(MaskColor, InColor, BackColor: TCnColor); overload;
    {* ����ǰͼ��ָ����ɫΪ��׼��ֵ��
     |<BR>
     |<BR> MaskColor: TCnColor    ָ������ɫ
     |<BR> InColor: TCnColor      ͼ������ָ��ɫ��ͬ�������ø���ɫ���
     |<BR> BackColor: TCnColor    ͼ������ָ��ɫ��ͬ�������ø���ɫ���}
    procedure Mask(MaskColor: TColor); overload;
    {* ����ǰͼ��ָ����ɫΪ��׼��ֵ��
     |<BR>
     |<BR> MaskColor: TColor    ָ������ɫ�������ɫ��ͬ�ı�Ϊ��ɫ����֮Ϊ��ɫ}
    procedure MaskEx(MaskColor, InColor, BackColor: TColor); overload;
    {* ����ǰͼ��ָ����ɫΪ��׼��ֵ��
     |<BR>
     |<BR> MaskColor: TColor    ָ������ɫ
     |<BR> InColor: TColor      ͼ������ָ��ɫ��ͬ�������ø���ɫ���
     |<BR> BackColor: TColor    ͼ������ָ��ɫ��ͬ�������ø���ɫ���}
    procedure AddColorNoise(Amount: Integer);
    {* �ڵ�ǰͼ�������Ӳ�ɫ������
     |<BR>
     |<BR> Amount: Integer    ����ϵ��}
    procedure AddMonoNoise(Amount: Integer);
    {* �ڵ�ǰͼ�������Ӻڰ�������
     |<BR>
     |<BR> Amount: Integer    ����ϵ��}
    procedure RemoveNoise(Amount: Integer);
    {* �ӵ�ǰͼ������ȥ�����㣬����ͼ���봦��
     |<BR>
     |<BR> Amount: Integer    ����ϵ��}
    procedure AddMiddleColor(Color: TColor);
    {* ����ǰͼ����ָ����ɫ����ֵ���㣬�������ɰ崦��
     |<BR>
     |<BR> Color: TColor    ǰ����ɫ}
    procedure AddMiddleColorEx(Color: TColor; Rect: TRect);
    {* ����ǰͼ���ָ��������ָ����ɫ����ֵ���㣬�������ɰ崦��
     |<BR>
     |<BR> Color: TColor    ǰ����ɫ
     |<BR> Rect: TRect      ָ������}

    // ����ͼ������
    procedure InterpolateRect(Rect: TRect; c00, c10, c01, c11: TCnColor); overload;
    {* �����Ľ���ɫֵ�ý���ɫ������
     |<BR>
     |<BR> Rect: TRect      ���ο�
     |<BR> c00: TCnColor    ���Ͻ���ɫ
     |<BR> c10: TCnColor    ���Ͻ���ɫ
     |<BR> c01: TCnColor    ���½���ɫ
     |<BR> c11: TCnColor    ���Ͻ���ɫ}
    procedure InterpolateRect(Rect: TRect; c00, c10, c01, c11: TColor); overload;
    {* �����Ľ���ɫֵ�ý���ɫ������
     |<BR>
     |<BR> Rect: TRect    ���ο�
     |<BR> c00: TColor    ���Ͻ���ɫ
     |<BR> c10: TColor    ���Ͻ���ɫ
     |<BR> c01: TColor    ���½���ɫ
     |<BR> c11: TColor    ���Ͻ���ɫ}

    // ����ݻ��ʻ��Ʒ�����֧��С����
    procedure DrawLineF(x1, y1, x2, y2: Single; Color: TColor);
    {* ��ָ����ɫ����һ��ֱ�ߣ�ʹ�ÿ�����㷨
     |<BR>
     |<BR> x1, y1: Single    ��ʼ������
     |<BR> x2, y2: Single    ����������
     |<BR> Color: TColor     ֱ����ɫ}
    procedure LineToF(x, y: Single); overload;
    {* �ӵ�ǰ��PenPosF����ֱ�ߵ�Ŀ��㣬ͬʱ�ƶ��������꣬ʹ�ÿ�����㷨
     |<BR>
     |<BR> x, y: Single      Ŀ�������}
    procedure LineToF(Point: TPointF); overload;
    {* �ӵ�ǰ��PenPosF����ֱ�ߵ�Ŀ��㣬ͬʱ�ƶ��������꣬ʹ�ÿ�����㷨
     |<BR>
     |<BR> Point: TPointF    Ŀ�������}
    procedure MoveToF(x, y: Single); overload;
    {* �ƶ���ǰ���ʵ�Ŀ���
     |<BR>
     |<BR> x, y: Single      Ŀ�������}
    procedure MoveToF(Point: TPointF); overload;
    {* �ƶ���ǰ���ʵ�Ŀ���
     |<BR>
     |<BR> Point: TPointF    Ŀ�������}
    procedure DrawRectF(const Rect: TRectF);
    {* ʹ�û��ʻ���һ�����Σ�ʹ�ÿ�����㷨
     |<BR>
     |<BR> Rect: TRectF    Ŀ�����}
    procedure PolylineF(const Points: TPointFArray);
    {* ʹ�û��ʻ������ߣ�ʹ�ÿ�����㷨
     |<BR>
     |<BR> Points: TPointFArray    ��������������}
    procedure EllipseF(x1, y1, x2, y2: Single); overload;
    {* ʹ�û��ʻ�����Բ��ʹ�ÿ�����㷨
     |<BR>
     |<BR> x1, y1: Single    ��Ӿ��ε����Ͻ�����
     |<BR> x2, y2: Single    ��Ӿ��ε����½�����}
    procedure EllipseF(const Rect: TRectF); overload;
    {* ʹ�û��ʻ�����Բ��ʹ�ÿ�����㷨
     |<BR>
     |<BR> Rect: TRectF    ��Ӿ���}

    // ƽ��������Ʒ���
    function TextExtent(const Text: string): TSize;
    {* �����ı���ʾ����ʹ��ƽ������ Font ���ԣ���֧�ֶ����ı�
     |<BR>
     |<BR> Text: string    �ı�����
     |<BR> Result: TSize    �ı�����}
    function TextHeight(const Text: string): Integer;
    {* �����ı���ʾ�߶ȣ�ʹ��ƽ������ Font ���ԣ���֧�ֶ����ı�
     |<BR>
     |<BR> Text: string      �ı�����
     |<BR> Result: Integer    �ı���ʾ�߶�}
    function TextWidth(const Text: string): Integer;
    {* �����ı���ʾ��ȣ�ʹ��ƽ������ Font ���ԣ���֧�ֶ����ı�
     |<BR>
     |<BR> Text: string      �ı�����
     |<BR> Result: Integer    �ı���ʾ���}
    procedure TextOut(x, y: Integer; const Text: string);
    {* �ڵ�ǰ��ǰͼ���л����ı���ʹ��ƽ������ Font ���ԣ���֧�ֶ����ı�
     |<BR> FontClear ���Ծ����ı������Ƿ�͸����FontBkColor Ϊ��͸��ʱ�ı������ɫ
     |<BR>
     |<BR> x, y: Integer    �ı����Ͻ�����
     |<BR> Text: string      �ı�����}

    // �߼�����
    property Handle: HBITMAP read GetHandle;
    {* ��ǰλͼ HBITMAP �����ֻ�����ԡ����λͼΪ�գ��������쳣}
    property DC: HDC read GetDC;
    {* ��ǰλͼ DC �����ֻ�����ԡ����λͼΪ�գ��������쳣}
    property Bits: Pointer read FBits;
    {* ��ǰλͼ�������ݴ�ŵĵ�ַ��ֻ�����ԡ����λͼΪ�գ�����nil}
    property Size: Integer read FSize;
    {* ��ǰλͼ�������ݿ�Ĵ�С��ֻ�����ԡ����λͼΪ�գ����� 0}
    property GdiAllocStyle: TGdiAllocStyle read FGdiAllocStyle write FGdiAllocStyle;
    {* ��ǰλͼ��GDI��Դ����ʽ���߼�����}
    property ScanLine[Row: Integer]: PCnLine read GetScanLine;
    {* ȡ�õ�ǰλͼһ��ɨ���ߵ�ַ��ֻ�����ԡ����λͼΪ�ջ�Χ���ޣ��������쳣}
    property Pixels[x, y: Integer]: TCnColor read GetPixel write SetPixel;
    {* ����λͼ�е�ĳ�����ء����λͼΪ�ջ�Χ���ޣ��������쳣}

    // ��������
    property Width: Integer read FWidth write SetWidth;
    {* ��ǰλͼ�Ŀ��}
    property Height: Integer read FHeight write SetHeight;
    {* ��ǰλͼ�ĸ߶�}
    property ClientRect: TRect read GetClientRect;
    {* ��ǰλͼ����������ֻ������}
    property Canvas: TCnCanvas read GetCanvas;
    {* ���ʵ�ǰλͼ�Ļ�����ֻ������}
    property Empty: Boolean read GetEmpty;
    {* ��ǰλͼ�Ƿ�Ϊ�գ�ֻ������}
    property Font: TCnFont read GetFont write SetFont;
    {* ƽ���������ԣ�������TFont���ṩһЩ��Ч��ʾ����}
    property FontClear: Boolean read FFontClear write FFontClear default False;
    {* ����ƽ�������ı�ʱ�������Ƿ�͸��}
    property FontBkColor: TColor read FFontBkColor write FFontBkColor default clWhite;
    {* ����ƽ�������ı�ʱ�����������͸����������䱳������ɫ}
    property SmoothFilter: Boolean read FSmoothFilter write FSmoothFilter default True;
    {* �ڶ�ͼ��������š���ת�ȼ��α任ʱ���Ƿ�ʹ�ÿ�����㷨����ƽ������}

    // �����ͼ�λ�������
    property PixelsF[x, y: Single]: TCnColor read GetPixelsF write SetPixelsF;
    {* ����λͼ�е�С����������ء����λͼΪ�ջ�Χ���ޣ��������쳣}
    property PenPosF: TPointF read FPenPosF write FPenPosF;
    {* �ڿ����ͼ�λ����У���ǰ���ʵ�λ��}
    property PenColor: TColor read FPenColor write FPenColor default clBlack;
    {* �ڿ����ͼ�λ����У���ǰ���ʵ���ɫ}
    property PenWeight: TPenWeight read FPenWeight write FPenWeight default pwNormal;
    {* �ڿ����ͼ�λ����У���ǰ���ʵĴ�ϸ�̶�}
  published
    property Transparent: Boolean read FTransparent write FTransparent default False;
    {* ͼ���͸�����ԣ������е�ͼ����ƹ�������Ч������ TransparentColor ���ж�}
    property TransparentColor: TColor read FTransparentColor write
      FTransparentColor default clDefault;
    {* ͼ���͸��ɫ���ԣ�λͼ�������ɫ��ͬ�����ص㰴͸������
     |<BR> ��ֵΪ clDefault ʱ��ʹ��ͼ�����½�������ɫֵ�����档}
  end;

procedure FreeBmpDC;
{* �ͷų���������λͼ�ѷ���� DC ���}
procedure FreeBmpHandle(All: Boolean);
{* �ͷų���������λͼ�ѷ���� HBITMAP ������������Ϊ�٣����� GdiAllocStyle �����ж�}

//--------------------------------------------------------//
// ��������ʱ����̿�                                     //
//--------------------------------------------------------//

var
  HSLRange: Integer = 240;

// HSL ��ɫ�� RGB ɫת������
function HSLToRGB(H, S, L: Double): TColor;
{* HSL ��ɫת��Ϊ RGB ��ɫ
 |<BR>
 |<BR> H, S, L: Double    �ֱ�Ϊɫ�������Ͷȡ����ȷ�����Ϊ"0"��"1"֮���С��
 |<BR> Result: TColor      ����RGB��ɫֵ}
function HSLRangeToRGB(H, S, L: Integer): TColor;
{* HSL ��ɫת��Ϊ RGB ��ɫ
 |<BR>
 |<BR> H, S, L: Integer    �ֱ�Ϊɫ�������Ͷȡ����ȷ�����0..240
 |<BR> Result: TColor      ����RGB��ɫֵ}
procedure RGBToHSL(Color: TColor; out H, S, L: Double);
{* RGB ��ɫת��Ϊ HSL ��ɫ
 |<BR>
 |<BR> Color: TColor      RGB ��ɫֵ
 |<BR> H, S, L: Integer    ����ֱ�Ϊɫ�������Ͷȡ����ȷ�����Ϊ"0"��"1"֮���С��}
procedure RGBToHSLRange(Color: TColor; out H, S, L: Integer);
{* RGB ��ɫת��Ϊ HSL ��ɫ
 |<BR>
 |<BR> Color: TColor      RGB ��ɫֵ
 |<BR> H, S, L: Integer    ����ֱ�Ϊɫ�������Ͷȡ����ȷ�����0..240}

// CMY ��ɫ�� RGB ɫת������
function CMYToRGB(const C, M, Y: Byte): TColor;
{* CMY ��ɫת��Ϊ RGB ��ɫ
 |<BR>
 |<BR> C, M, Y: Byte      �ֱ�Ϊ Cyan �ࡢMagenta Ʒ�졢Yellow �Ʒ�����0..255
 |<BR> Result: TColor      ���� RGB ��ɫֵ}
procedure RGBToCMY(const RGB: TColor; out C, M, Y: Byte);
{* RGB ��ɫת��Ϊ CMY ��ɫ
 |<BR>
 |<BR> Color: TColor      RGB ��ɫֵ
 |<BR> C, M, Y: Byte      ����ֱ�Ϊ Cyan �ࡢMagenta Ʒ�졢Yellow �Ʒ�����0..255}

// CMYK ��ɫ�� RGB ɫת������
function CMYKToRGB(const C, M, Y, K: Byte): TColor;
{* CMYK ��ɫת��Ϊ RGB ��ɫ
 |<BR>
 |<BR> C, M, Y, K: Byte    �ֱ�Ϊ Cyan �ࡢMagenta Ʒ�졢Yellow �ơ�Black �ڷ�����0..255
 |<BR> Result: TColor      ���� RGB ��ɫֵ}
procedure RGBToCMYK(const RGB: TColor; out C, M, Y, K: Byte);
{* RGB ��ɫת��Ϊ CMY ��ɫ
 |<BR>
 |<BR> Color: TColor      RGB ��ɫֵ
 |<BR> C, M, Y, K: Byte    ����ֱ�Ϊ Cyan �ࡢMagenta Ʒ�졢Yellow �ơ�Black �ڷ�����0..255}

// ��ǿ����ɫ������
function Gray(Intensity: Byte): TColor;
{* ����һ���Ҷ� RGB ��ɫֵ}
function Intensity(Color: TColor): Byte;
{* ���� RGB ��ɫֵ�ĻҶ�ֵ}
function RandomColor: TColor;
{* ����һ����� RGB ��ɫֵ}
procedure DeRGB(Color: TColor; var r, g, b: Byte);
{* �� Color �ֽ�Ϊ r��g��b ��ɫ����}

// CnColor��ɫ������
function CnColor(r, g, b: Byte): TCnColor; overload;
{* ���� r��g��b ��ɫ��������һ�� TCnColor ��ɫֵ}
function CnColor(Color: TColor): TCnColor; overload;
{* ת�� TColor ��ɫΪһ�� TCnColor ��ɫֵ������ʹ��ϵͳ��ɫֵ}
function CnGray(Intensity: Byte): TCnColor;
{* ����һ���Ҷȼ��� TCnColor ��ɫֵ}
function CnWinColor(RGB: TCnColor): TColor;
{* ת�� TCnColor ��ɫΪһ�� TColor ��ɫֵ}
function CnColorEqu(RGB1, RGB2: TCnColor): Boolean;
{* �ж����� TCnColor ��ɫ�Ƿ����}

function PointF(x, y: Single): TPointF;
{* ����һ������������ TPointF}
function RectF(Left, Top, Right, Bottom: Single): TRectF;
{* ����һ������������ TRectF}

// �Ӹ��ؼ����Ʊ���������������� RxLibrary VCLUtils��ע�Ⲣ��������Ч�������� IDE �༭����
procedure CopyControlParentImageToCanvas(AControl: TControl; Dest: TCanvas);

// ���þ����
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
  BitmapList: TThreadList;    // TCnBitmap λͼ�б�
  CnCanvasList: TThreadList;  // TCnCanvas �б�
  GdiActTimer: TTimer;        // GDI��Դ�ͷŶ�ʱ��
  DefGdiAllocStyle: TGdiAllocStyle = gsNormal; // Ĭ��GDI�ͷŷ�ʽ
  FreeGdiWaitTime: Cardinal = 3000; // �Զ��ͷ�GDI��Դ�ȴ�ʱ��

const
  FreeGdiInterval: Cardinal = 1000; // �Զ��ͷ�GDI��Դ��ʱ���
  csItalicAdjust = 0.3;       // б���ֿ��У��ϵ��

type
  TLogPal = record
    lpal: TLogPalette;
    dummy: array[0..255] of TPaletteEntry;
  end;

var
  GrayLogPal: TLogPal;

//--------------------------------------------------------//
// ��������ʱ����̿�                                     //
//--------------------------------------------------------//

// HSL��RGB ת�������㷨��Դ��
// http:/www.r2m.com/win-developer-faq/graphics/8.html
// Grahame Marsh 12 October 1997

// HSL ��ɫת��Ϊ RGB ɫ
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

// HSL ��ɫ��Χת��Ϊ RGB ɫ
function HSLRangeToRGB(H, S, L: Integer): TColor;
begin
  Result := HSLToRGB(H / (HSLRange - 1), S / HSLRange, L / HSLRange)
end;

// RGB ��ɫתΪ HSL ɫ
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

// RGB ��ɫתΪ HSL ɫ��Χ
procedure RGBToHSLRange(Color: TColor; out H, S, L: Integer);
var
  Hd, Sd, Ld: Double;
begin
  RGBToHSL(Color, Hd, Sd, Ld);
  H := Round(Hd * (HSLRange - 1));
  S := Round(Sd * HSLRange);
  L := Round(Ld * HSLRange);
end;

// CMY ��ɫ�� RGB ɫת������
// �㷨�ṩ��CnPack ������ ����

// CMY ��ɫת��Ϊ RGB
function CMYToRGB(const C, M, Y: Byte): TColor;
var
  r, g, b: Byte;
begin
  r := 255 - C;
  g := 255 - M;
  b := 255 - Y;
  Result := RGB(r, g, b);
end;

// RGB ��ɫת��Ϊ CMY
procedure RGBToCMY(const RGB: TColor; out C, M, Y: Byte);
var
  r, g, b: Byte;
begin
  DeRGB(RGB, r, g, b);
  C := 255 - r;
  M := 255 - g;
  Y := 255 - b;
end;

// CMYK ��ɫ�� RGB ɫת������
// �㷨�ṩ��CnPack ������ ����

// CMYK ��ɫת��Ϊ RGB
function CMYKtoRGB(const C, M, Y, K: Byte): TColor;
var
  r, g, b: Byte;
begin
  r := 255 - (C + K);
  g := 255 - (M + K);
  b := 255 - (Y + K);
  Result := RGB(r, g, b);
end;

// RGB ��ɫת��Ϊ CMYK
procedure RGBToCMYK(const RGB: TColor; out C, M, Y, K: Byte);
begin
  RGBToCMY(RGB, C, M, Y);
  K := MinIntValue([C, M, Y]);
  C := C - K;
  M := M - K;
  Y := Y - K;
end;

// �����Ҷ���ɫ
function Gray(Intensity: Byte): TColor;
begin
  Result := Intensity shl 16 + Intensity shl 8 + Intensity;
end;

// ������ɫ����ֵ
// �㷨��Դ��Graphic32
// �㷨�޸ģ��ܾ���
function Intensity(Color: TColor): Byte;
asm
// ����:  RGB --> EAX
// ���:  (R * 61 + G * 174 + B * 20) / 256 --> AL
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

// ���������ɫ
function RandomColor: TColor;
begin
  Result := HSLToRGB(Random, 0.75 + Random * 0.25, 0.3 + Random * 0.25);
end;

// ȡ��ɫRGB����
procedure DeRGB(Color: TColor; var r, g, b: Byte);
begin
  Color := ColorToRGB(Color);
  r := GetRValue(Color);
  g := GetGValue(Color);
  b := GetBValue(Color);
end;

// CnColor ��ɫ������
function CnColor(r, g, b: Byte): TCnColor;
begin
  Result.r := r;
  Result.g := g;
  Result.b := b;
end;

// ϵͳ��ɫתΪ TCnColor
function CnColor(Color: TColor): TCnColor;
begin
  Color := ColorToRGB(Color);
  Result.r := Color;
  Result.g := Color shr 8;
  Result.b := Color shr 16;
end;

// �����Ҷȼ� TCnColor
function CnGray(Intensity: Byte): TCnColor;
begin
  Result.r := Intensity;
  Result.g := Intensity;
  Result.b := Intensity;
end;

// TCnColor תΪ TColor
function CnWinColor(RGB: TCnColor): TColor;
begin
  Result := RGB.b shl 16 + RGB.g shl 8 + RGB.r;
end;

// ��ɫֵ���
function CnColorEqu(RGB1, RGB2: TCnColor): Boolean;
begin
  Result := (RGB1.r = RGB2.r) and (RGB1.g = RGB2.g) and (RGB1.b = RGB2.b);
end;

// ȡ��
function PointF(x, y: Single): TPointF;
begin
  Result.x := x;
  Result.y := y;
end;

// ȡ����
function RectF(Left, Top, Right, Bottom: Single): TRectF;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Right;
  Result.Bottom := Bottom;
end;

//--------------------------------------------------------//
// ˽�й��̿�                                             //
//--------------------------------------------------------//

// ��Χת��
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

// ��Χת��Ϊʵ��ֵ
function RangeToInt(Range: TAdjustRange; Min, Max: Integer): Integer;
begin
  Result := RangeTran(Range, Low(Range), High(Range), Min, Max);
end;

// ͸����ת��
function AlphaToInt(Alpha: TCnAlpha): Integer;
begin
  Result := RangeTran(Alpha, Low(Alpha), High(Alpha), 0, 255);
end;

// ȡ������ת��֮��Ӿ�����Ŀ�����֮��
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

  // ����Ŀ�궥��λ��
  SrcW2 := W / 2 + 1;
  SrcH2 := H / 2 + 1;
  wCos := SrcW2 * cAngle;
  hCos := SrcH2 * cAngle;
  wSin := SrcW2 * sAngle;
  hSin := SrcH2 * sAngle;
  p1.x := Round(-wCos - hSin + DstCenter.x); // ����
  p1.y := Round(-wSin + hCos + DstCenter.y);
  p2.x := Round(wCos - hSin + DstCenter.x); // ����
  p2.y := Round(wSin + hCos + DstCenter.y);
  p3.x := Round(-wCos + hSin + DstCenter.x); // ����
  p3.y := Round(-wSin - hCos + DstCenter.y);
  p4.x := Round(wCos + hSin + DstCenter.x); // ����
  p4.y := Round(wSin - hCos + DstCenter.y);

  // �����������
  Rect.Left := MinIntValue([p1.x, p2.x, p3.x, p4.x]) - 1;
  Rect.Right := MaxIntValue([p1.x, p2.x, p3.x, p4.x]) + 1;
  Rect.Top := MinIntValue([p1.y, p2.y, p3.y, p4.y]) - 1;
  Rect.Bottom := MaxIntValue([p1.y, p2.y, p3.y, p4.y]) + 1;
  Result := IntersectRect(Rect, Rect, DstRect);
end;

//--------------------------------------------------------//
// ������ɫ��                                             //
//--------------------------------------------------------//

{ TCnMiddleColorItem }

// ��ֵ
procedure TCnMiddleColorItem.Assign(Source: TPersistent);
begin
  if Source is TCnMiddleColorItem then
  begin
    FColor := TCnMiddleColorItem(Source).FColor;
    FPos := TCnMiddleColorItem(Source).FPos;
    Changed(False);
  end
  else
    inherited;                // TCollectionItem δʵ�ָ÷���
end;

// ��ʼ��
constructor TCnMiddleColorItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FColor := clBlack;
  FPos := 50;
end;

// ����ɫֵ
procedure TCnMiddleColorItem.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed(False);
  end;
end;

// ��λ��ֵ
procedure TCnMiddleColorItem.SetPos(const Value: TCnGradPos);
begin
  if FPos <> Value then
  begin
    FPos := Value;
    Changed(False);
  end;
end;

{ TCnMiddleColor }

// ��ʼ��
constructor TCnMiddleColor.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TCnMiddleColorItem);
  FSorting := False;
end;

// ����һ��
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

// ��λ������
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
    if GetOwner is TCnPersistent then // ֪ͨ����
      TCnPersistentAccess(GetOwner).Changed;
  finally
    EndUpdate;
    FSorting := False;
  end;
end;

// �����Ѹ���
procedure TCnMiddleColor.Update(Item: TCollectionItem);
begin
  inherited;
  Sort;
end;

// ����ķ���������������Ա༭����
// ��ʾ�����Ա༭���е�����
function TCnMiddleColor.GetAttr(Index: Integer): string;
begin
  case Index of
    0: Result := 'Color';
    1: Result := 'Position';
  else Result := inherited GetAttr(Index);
  end;
end;

// ��ʾ�����Ա༭���е�����
function TCnMiddleColor.GetAttrCount: Integer;
begin
  Result := 2;
end;

// ��ʾ�����Ա༭���е���������
function TCnMiddleColor.GetItemAttr(Index, ItemIndex: Integer): string;
begin
  case Index of
    0: Result := ColorToString(Items[ItemIndex].FColor);
    1: Result := Format('[%d%%]', [Items[ItemIndex].FPos]);
  else Result := inherited GetItemAttr(Index, ItemIndex);
  end;
end;

// ȡ����
function TCnMiddleColor.GetItem(Index: Integer): TCnMiddleColorItem;
begin
  Result := TCnMiddleColorItem(inherited GetItem(Index));
end;

// ������
procedure TCnMiddleColor.SetItem(Index: Integer;
  const Value: TCnMiddleColorItem);
begin
  inherited SetItem(Index, Value);
end;

{ TCnGradientColor }

// ��ֵ
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

// ��ʼ��
constructor TCnGradientColor.Create;
begin
  inherited;
  FColorStart := clBlack;
  FColorEnd := clBlack;
  FStyle := gsLeftToRight;
  FColorMiddle := nil;
end;

// �ͷ�
destructor TCnGradientColor.Destroy;
begin
  if FColorMiddle <> nil then
    FreeAndNil(FColorMiddle);
  inherited;
end;

// ȡ�м�ɫ
function TCnGradientColor.GetColorMiddle: TCnMiddleColor;
begin
  if FColorMiddle = nil then
    FColorMiddle := TCnMiddleColor.Create(Self);
  Result := FColorMiddle;
end;

// �����ɫ
procedure TCnGradientColor.SetColorEnd(const Value: TColor);
begin
  if FColorEnd <> Value then
  begin
    FColorEnd := Value;
    Changed;
  end;
end;

// ���м�ɫ
procedure TCnGradientColor.SetColorMiddle(const Value: TCnMiddleColor);
begin
  BeginUpdate;
  try
    if (Value <> nil) and (Value.Count > 0) then
      ColorMiddle.Assign(Value) // �Զ�����Get������֤ʵ����
    else if FColorMiddle <> nil then // ���м�ɫʱ�ͷ�ʵ��
      FreeAndNil(FColorMiddle);
  finally
    EndUpdate;
  end;
end;

// ����ʼɫ
procedure TCnGradientColor.SetColorStart(const Value: TColor);
begin
  if FColorStart <> Value then
  begin
    FColorStart := Value;
    Changed;
  end;
end;

// �轥����
procedure TCnGradientColor.SetStyle(const Value: TCnGradStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Changed;
  end;
end;

//--------------------------------------------------------//
// ƽ����Ч������                                         //
//--------------------------------------------------------//

{ TCnShadow }

// ��ֵ
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

// ��ʼ��
constructor TCnShadow.Create;
begin
  inherited;
  FBlur := 1;
  FAlpha := 180;
  FColor := $00444444;
  FOffsetX := 2;
  FOffsetY := 2;
end;

// ���ò�͸����
procedure TCnShadow.SetAlpha(const Value: TCnAlpha);
begin
  if FAlpha <> Value then
  begin
    FAlpha := Value;
    Changed;
  end;
end;

// ������Ӱģ��
procedure TCnShadow.SetBlur(const Value: TShadowBlur);
begin
  if FBlur <> Value then
  begin
    FBlur := Value;
    Changed;
  end;
end;

// ������Ӱɫ
procedure TCnShadow.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

// ������Ӱˮƽƫ��
procedure TCnShadow.SetOffsetX(const Value: TShadowOffset);
begin
  if FOffsetX <> Value then
  begin
    FOffsetX := Value;
    Changed;
  end;
end;

// ������Ӱ��ֱƫ��
procedure TCnShadow.SetOffsetY(const Value: TShadowOffset);
begin
  if FOffsetY <> Value then
  begin
    FOffsetY := Value;
    Changed;
  end;
end;

{ TCnLighting }

// ��ֵ
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

// ��ʼ��
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

// ���ò�͸����
procedure TCnLighting.SetAlpha(const Value: TCnAlpha);
begin
  if FAlpha <> Value then
  begin
    FAlpha := Value;
    Changed;
  end;
end;

// ���ýǶ�
procedure TCnLighting.SetAngle(const Value: Double);
begin
  if FAngle <> Value then
  begin
    FAngle := Value;
    Changed;
  end;
end;

// ���õƹ���ɫ
procedure TCnLighting.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

// ���ù��շ�Χ���
procedure TCnLighting.SetWidth(const Value: TLightingRange);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    Changed;
  end;
end;

// ���ù��շ�Χ�߶�
procedure TCnLighting.SetHeight(const Value: TLightingRange);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    Changed;
  end;
end;

// ���ù�������ƫ��
procedure TCnLighting.SetOffsetX(const Value: TLightingOffset);
begin
  if FOffsetX <> Value then
  begin
    FOffsetX := Value;
    Changed;
  end;
end;

// ���ù�������ƫ��
procedure TCnLighting.SetOffsetY(const Value: TLightingOffset);
begin
  if FOffsetY <> Value then
  begin
    FOffsetY := Value;
    Changed;
  end;
end;

{ TCnFont }

// ��ֵ
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

// ��ʼ��
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

// �ͷ�
destructor TCnFont.Destroy;
begin
  FLighting.Free;
  FGradient.Free;
  FShadow.Free;
  if FTexture <> nil then FTexture.Free;
  inherited;
end;

// ��ʼ����
procedure TCnFont.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

// ȡ������
function TCnFont.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

// ��������
procedure TCnFont.EndUpdate;
begin
  Assert(FUpdateCount > 0, 'Unpaired TCnFont.EndUpdate');
  Dec(FUpdateCount);
  if (FUpdateCount = 0) then Changed;
end;

// �����ѱ��
procedure TCnFont.Changed;
begin
  if FUpdateCount = 0 then inherited;
end;

// ��Ԫ�ظ���֪ͨ
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

// ȡ����
function TCnFont.GetTexture: TPicture;
begin
  if FTexture = nil then
  begin
    FTexture := TPicture.Create;
    FTexture.OnChange := ChildChanged;
  end;
  Result := FTexture;
end;

// ����͸����
procedure TCnFont.SetAlpha(const Value: TCnAlpha);
begin
  if FAlpha <> Value then
  begin
    FAlpha := Value;
    Changed;
  end;
end;

// ���ý���ɫ
procedure TCnFont.SetGradient(const Value: TCnGradientColor);
begin
  BeginUpdate;
  try
    FGradient.Assign(Value);
  finally
    EndUpdate;
  end;
end;

// ��������
procedure TCnFont.SetNoise(const Value: Byte);
begin
  if FNoise <> Value then
  begin
    FNoise := Value;
    Changed;
  end;
end;

// �����罦
procedure TCnFont.SetSpray(const Value: Byte);
begin
  if FSpray <> Value then
  begin
    FSpray := Value;
    Changed;
  end;
end;

// ������Ӱ
procedure TCnFont.SetShadow(const Value: TCnShadow);
begin
  BeginUpdate;
  try
    FShadow.Assign(Value);
  finally
    EndUpdate;
  end;
end;

// ���ù���
procedure TCnFont.SetLighting(const Value: TCnLighting);
begin
  BeginUpdate;
  try
    FLighting.Assign(Value);
  finally
    EndUpdate;
  end;
end;

// ������չ���
procedure TCnFont.SetStyleEx(const Value: TFontStyleExs);
begin
  if FStyleEx <> Value then
  begin
    FStyleEx := Value;
    Changed;
  end;
end;

// ��������
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

// ��������ģʽ
procedure TCnFont.SetTextureMode(const Value: TCnDrawMode);
begin
  if FTextureMode <> Value then
  begin
    FTextureMode := Value;
    Changed;
  end;
end;

// ������ʾ����
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

// ģ������
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

// ��˹ģ������
procedure TCnFontMask.Blur(Amount: Integer);
var
  I: Integer;
begin
  if Amount > 0 then
    for I := Amount downto 1 do
      SplitBlur(I);
end;

// �罦Ч��
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

// ����
procedure TCnFontMask.CopyTo(Dst: TCnFontMask);
begin
  Dst.SetSize(FWidth, FHeight);
  Move(FBuff^, Dst.Buff^, FRowInc * FHeight);
end;

// �ͷ�
destructor TCnFontMask.Destroy;
begin
  SetSize(0, 0);
  inherited;
end;

// ��������Sobel���ӱ�Ե��⣩
procedure TCnFontMask.Outline;
var
  x, y: Integer;
  s1, s2, s3, s4, Sum: Integer;
  Tmp: TCnFontMask;
  pDst: PByteArray;
  pUp, pMiddle, pDown: PByteArray; //�����ָ��
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

// ɨ����
function TCnFontMask.GetScanLine(Row: Integer): PByteArray;
begin
  Result := PByteArray(TCnNativeInt(FBuff) + Row * FRowInc);
end;

// ���ô�С
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
// ����ͼ������                                         //
//--------------------------------------------------------//

{ TCnCanvas }

// ��ʼ��
constructor TCnCanvas.Create(ABitmap: TCnBitmap);
begin
  inherited Create;
  FDC := 0;
  FBitmap := ABitmap;
end;

// �ͷ�
destructor TCnCanvas.Destroy;
begin
  FreeContext;
  inherited;
end;

// ȡ DC
function TCnCanvas.GetHandle: HDC;
begin
  Result := inherited Handle; // ȡ�̳ж����ľ��
end;

// ���� DC�����ط������� TCanvas �ڲ�����
procedure TCnCanvas.CreateHandle;
var
  H: HDC;
begin
  Lock;
  try
    FBitmap.HandleNeeded;     // ����λͼ���
    H := CreateCompatibleDC(0); // ��������DC
    if H = 0 then raise ECnGraphics.Create(SCreateDCFail);
    if SelectObject(H, FBitmap.FHandle) = 0 then // ѡ��λͼ���ڲ�DC
      raise ECnGraphics.Create(SSelectBmpToDCFail);
    FDC := H;
    inherited Handle := H;    // ���ü̳ж����ľ��
    CnCanvasList.Add(Self);   // ���������б���
  finally
    Unlock;
  end;
end;

// �ͷ�DC
procedure TCnCanvas.FreeContext;
begin
  if FDC <> 0 then
  begin
    Lock;
    try
      inherited Handle := 0;  // �ͷż̳ж����ľ��
      DeleteDC(FDC);          // ɾ��DC
      FDC := 0;
      CnCanvasList.Remove(Self); // ���б���ɾ��
    finally
      Unlock;
    end;
  end;
end;

{ TCnBitmap }

// ��ʼ��
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

// �ͷ�
destructor TCnBitmap.Destroy;
begin
  Lock;                       // ���߳�ͬ��
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
// ��̬λͼ��Դ��DC������                               //
// �㷨��ƣ��ܾ���                                       //
// �㷨�ο���Graphic32��pnBitmap��Graphic.pas             //
//--------------------------------------------------------//

// �ͷ������ڴ�DC
procedure FreeBmpDC;
var
  I: Integer;
  Canvas: TCnCanvas;
begin
  with CnCanvasList.LockList do // ��������߳�Ҳ�ڷ�����ȴ�
  try
    for I := Count - 1 downto 0 do
    begin
      Canvas := TCnCanvas(Items[I]);
      if Canvas.TryLock then
      try
        Canvas.FreeContext;   // �ͷ�DC
      finally
        Canvas.Unlock;
      end;
    end;
  finally
    CnCanvasList.UnlockList;
  end;
end;

// �ͷ�λͼ���
procedure FreeBmpHandle(All: Boolean);
var
  I: Integer;
  Bmp: TCnBitmap;
begin
  with BitmapList.LockList do // ��������߳�Ҳ�ڷ�����ȴ�
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
          Bmp.HandleRelease;  // �ͷ�λͼ���
        finally
          Bmp.Unlock;
        end;
      end;
    end;
  finally
    BitmapList.UnlockList;
  end;
end;

// λͼ��Դ��ʱ�����෽����
// TTimer.OnTimerҪ��һ�����󷽷����ʶ�����෽��
class procedure TCnBitmap.OnGdiActTimer(Sender: TObject);
begin
  FreeBmpDC;
  FreeBmpHandle(False);
end;

// ȡCanvas
function TCnBitmap.GetCanvas: TCnCanvas;
begin
  if FCanvas = nil then       // ��һ�η���ʱ����
  begin
    FCanvas := TCnCanvas.Create(Self);
    FCanvas.OnChange := OnChildChange;
    FCanvas.OnChanging := OnChildChanging;
  end;
  FGdiLastAccess := GetTickCount; // �����ʻ�����ʱ��
  Result := FCanvas;
end;

// �ͷž������Դ�������������������
procedure TCnBitmap.CleanUp;
begin
  if HandleAllocated then     // �ѷ���λͼ���
    HandleRelease(False)      // �ͷ�λͼ��Դ
  else
  begin
    if Assigned(FBits) then FreeMem(FBits); // �ͷ�λͼ���ݿ�
    if Assigned(FScanLine) then FreeMem(FScanLine); // ɾ��ɨ��������
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

// �ͷž���������
procedure TCnBitmap.FreeImage;
begin
  CleanUp;
  Changed;
end;

// ����ɨ����ָ������
procedure TCnBitmap.UpdateScanLine;
var
  I: Integer;
  x: TCnNativeInt;
begin
  ReallocMem(FScanLine, FHeight * SizeOf(PCnLine)); // ���·���ɨ����ָ������ռ�
  x := TCnNativeInt(FBits);
  for I := 0 to Height - 1 do
  begin
    FScanLine[I] := Pointer(x); // ��ʼ��ɨ����ָ����������
    Inc(x, FRowInc);
  end;
end;

// GDI ����ѷ���
function TCnBitmap.HandleAllocated: Boolean;
begin
  Result := FHandle <> 0;
end;

// ��Ҫ HBITMAP ���
procedure TCnBitmap.HandleNeeded;
var
  Tmp: Pointer;
begin
  if HandleAllocated then Exit;
  if Empty then               // ����Ϊ��λͼ������
    raise EBitmapIsEmpty.Create(SCreateDCFromEmptyBmp);

  Lock;
  try
    FillChar(FBitmapInfo, SizeOf(TBitmapInfo), 0);
    with FBitmapInfo.bmiHeader do // ��ʼ��FBitmapInfo
    begin
      biSize := SizeOf(TBitmapInfoHeader);
      biWidth := Width;
      biHeight := -Height;    // �߶�Ϊ����ɨ���߰��͵�ַ���ߵ�ַ��ʽ���
      biPlanes := 1;
      biBitCount := 24;       // 24 Bit RGB λͼ
      biCompression := BI_RGB;
    end;                      // ԭͼ������
    Tmp := FBits;             // ����λͼ
    FHandle := CreateDIBSection(0, FBitmapInfo, DIB_RGB_COLORS, FBits, 0, 0);
    if FHandle = 0 then       // �޷�����λͼ�����
      raise ECnGraphics.Create(SAllocDIBFail);
    Move(Tmp^, FBits^, Size); // �ƶ�ԭ���ݵ���������
    FreeMem(Tmp);             // �ͷ�ԭ���ݿ�
    UpdateScanLine;           // ����ɨ����ָ����������
    BitmapList.Add(Self);     // ���ӵ�λͼ�б�
  finally
    Unlock;
  end;
end;

// �ͷ�HBITMAPλͼ���������ΪTrue������λͼ���ݣ�
procedure TCnBitmap.HandleRelease(KeepData: Boolean);
var
  Tmp: Pointer;
begin
  if not HandleAllocated then Exit;

  if Assigned(FCanvas) then
    TCnCanvas(FCanvas).FreeContext; // �ͷ� DC
  Lock;
  try
    if KeepData then          // �������ݣ���ɾ��λͼ���
    begin
      Tmp := FBits;
      GetMem(FBits, FSize);
      Move(Tmp^, FBits^, FSize);
      UpdateScanLine;         // ����ɨ����ָ����������
    end else
    begin                     // ͬʱ�ͷ�λͼ����
      if Assigned(FScanLine) then FreeMem(FScanLine); // ɾ��ɨ��������
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
    DeleteObject(FHandle);    // ɾ��λͼ����
    FHandle := 0;
    BitmapList.Remove(Self);  // ��λͼ�б���ɾ��
  finally
    Unlock;
  end;
end;

// ȡλͼ��� HBITMAP
function TCnBitmap.GetHandle: HBITMAP;
begin
  if not HandleAllocated then HandleNeeded;
  Result := FHandle;
end;

// ȡ�� DC
function TCnBitmap.GetDC: HDC;
begin
  Result := Canvas.Handle;
end;

// ȡͼ���״̬
function TCnBitmap.GetEmpty: Boolean;
begin
  Result := FBits = nil;
end;

// ����ͼ��ߴ�
procedure TCnBitmap.SetSize(AWidth, AHeight: Integer);
begin
  if AWidth < 0 then AWidth := 0;
  if AHeight < 0 then AHeight := 0;
  if (AWidth = Width) and (AHeight = Height) then Exit;

  Changing;
  CleanUp;                    // �ͷ�ԭ����
  FWidth := AWidth;
  FHeight := AHeight;
  if (AWidth > 0) and (AHeight > 0) then
  begin
    FRowInc := (FWidth * 3) + FWidth mod 4; // ÿ��ɨ���߳��ȣ��� 4 �ֽڶ���
    FGap := FWidth mod 4;       // ÿ��ɨ����������Ч����
    FSize := FRowInc * FHeight; // ͼ���С
    GetMem(FBits, FSize);       // ����ͼ��ռ�
    UpdateScanLine;             // ����ɨ����ָ����������
    //Fill(CnWinColor(GetTranColor));  // ��͸��ɫ���
  end;
  Changed;
end;

// ����ͼ��ߴ磨TRect ���Ͳ�����
procedure TCnBitmap.SetSize(ARect: TRect);
begin
  SetSize(ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);
end;

// ���ø߶�
procedure TCnBitmap.SetHeight(const Value: Integer);
begin
  if FHeight <> Value then SetSize(FWidth, Value);
end;

// ���ÿ��
procedure TCnBitmap.SetWidth(const Value: Integer);
begin
  if FWidth <> Value then SetSize(Value, FHeight);
end;

//--------------------------------------------------------//
// ��������                                               //
//--------------------------------------------------------//

// ����ѹ��������Ŀ�����ݳ���
function TCnBitmap.Compress(var pData: Pointer; ASize: Integer): Integer;
begin
  Result := ASize;            { TODO -o�ܾ��� -cѹ����ѹ : λͼ����ѹ����ѹ�� }
end;

// ���ݽ�ѹ������Ŀ�����ݳ��ȣ�������-1��
function TCnBitmap.DeCompress(var pData: Pointer; ASize: Integer): Integer;
begin
  Result := ASize;
end;

// ȡ���ƾ�����
function TCnBitmap.GetClientRect: TRect;
begin
  Result := Rect(0, 0, Width, Height);
end;

// ȡ͸��ɫ
function TCnBitmap.GetTranColor: TCnColor;
begin
  if TransparentColor <> clDefault then
    Result := CnColor(TransparentColor)
  else if not Empty then
    Result := Pixels[0, Height - 1] // Ĭ��Ϊ���½����أ��� TBitmap ����һ�£�
  else
    Result := CnColor(0, 0, 0);
end;

// ȡƽ������
function TCnBitmap.GetFont: TCnFont;
begin
  if FFont = nil then
  begin
    FFont := TCnFont.Create;
    FFont.OnChange := OnChildChange;
  end;
  Result := FFont;
end;

// ����ƽ������
procedure TCnBitmap.SetFont(const Value: TCnFont);
begin
  if Value <> nil then
    Font.Assign(Value);       // �Զ����� GetFont ������֤ʵ����
end;

// ��ָ��ɫ���λͼ
procedure TCnBitmap.Fill(Color: TColor);
begin
  FillRect(ClientRect, Color);
end;

// ��ָ��ɫ����������
// �㷨���ܾ���
procedure TCnBitmap.FillRect(Rect: TRect; Color: TColor);
var
  x, y, w, h, I, lw: Integer;
  ARect: TRect;
  Tmp: PCnColor;
  ARGB: TCnColor;
begin
  if Empty then
    Exit;         // ����Ч����
  if not IntersectRect(ARect, Rect, ClientRect) then Exit;
  Changing;
  if (Color = clBlack) and RectEqu(ARect, ClientRect) then
    FillChar(Bits^, Size, 0)  // ȫ��������
  else
  begin
    DeRect(ARect, x, y, w, h);
    lw := w * 3;              // �洢���
    ARGB := CnColor(Color);   // ת��Ϊ RGB ��ʽ
    Tmp := @FScanLine[y][x];
    if Color = clBlack then   // ��ɫȫ������
      FillChar(Tmp^, lw, 0)
    else
    begin
      for I := 0 to w - 1 do  // ����һ��ɨ����
      begin
        Tmp^ := ARGB;
        Inc(Tmp);
      end;
    end;
    Tmp := @FScanLine[y][x];
    for I := y + 1 to y + h - 1 do // ���Ƶ�����ɨ����
      Move(Tmp^, (@FScanLine[I][x])^, lw);
  end;
  Changed;
end;

// ��ָ��ɫ����
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

// ��ָ��ɫ������
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

// �Ե�ǰͼ�񴴽�һ�� Region
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
  // ���� Region ����
  Result := Count * SizeOf(TRect);
  GetMem(Rgndata, SizeOf(TRgnDataHeader) + Result);
  FillChar(Rgndata^, SizeOf(TRgnDataHeader) + Result, 0);
  RgnData^.rdh.dwSize := SizeOf(TRgnDataHeader);
  RgnData^.rdh.iType := RDH_RECTANGLES;
  RgnData^.rdh.nCount := Count;
  RgnData^.rdh.nRgnSize := 0;
  RgnData^.rdh.rcBound := Rect(0, 0, Width, Height);
  // ����Region
  Move(Rts, RgnData^.Buffer, Result);
  Result := SizeOf(TRgnDataHeader) + Count * SizeOf(TRect);
end;

//--------------------------------------------------------//
// ��ֵ����                                               //
// �㷨��Դ��Graphic32��pnBitmap                          //
// �㷨�޸ģ��ܾ���                                       //
//--------------------------------------------------------//

// ��ֵ����
procedure TCnBitmap.Assign(Source: TPersistent);
begin
  BeginUpdate;
  try
    if Source = nil then      // ����ֵ�ÿ�
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
    else if Source is TGraphic then // TIcon��TJpegImage �ȵ�
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
    if GdiAllocStyle = gsInternal then HandleRelease; // �ͷž��
    EndUpdate;
  end;
end;

// ��ֵ��Ŀ�꣨����������
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
    else if Dest is TGraphic then // TIcon��TJpegImage �ȵ�
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
    if GdiAllocStyle = gsInternal then HandleRelease; //�ͷž��
  end;
end;

//--------------------------------------------------------//
// �洢��������                                           //
// �㷨��Դ��Delphi Graphic.pas                           //
// �㷨�޸ģ��ܾ���                                       //
//--------------------------------------------------------//

// ����洢���ԣ������� DFM �е��Զ������ԣ�
procedure TCnBitmap.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  begin
    if Filer.Ancestor <> nil then // �Ǵӻ�����̳ж���
      Result := not (Filer.Ancestor is TCnBitmap) or
        not Equals(TCnBitmap(Filer.Ancestor))
    else
      Result := not Empty;
  end;
begin                         // �������� Data ����ͼ������
  Filer.DefineBinaryProperty('Data', ReadData, WriteData, DoWrite);
end;

// �� DFM ���ж����ݹ���
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
    if DeCompress(Buff, ASize) > 0 then // ���ݽ�ѹ
      LoadFromMemory(Buff, AWidth, AHeight)
    else
      raise ECnGraphics.Create(SReadBmpError);
  finally
    FreeMem(Buff);
  end;
end;

// д���ݵ� DFM ������
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
      Move(Bits^, Buff^, Size);      // ��ʱ����
      ASize := Compress(Buff, Size); // ѹ������
      Stream.Write(ASize, SizeOf(ASize));
      Stream.Write(Buff^, ASize);
    finally
      FreeMem(Buff);
    end;
  end;
end;

// �ж�����ͼ�������Ƿ�ȫ��
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

//--------------------------------------------------------//
// ���ط����ô���                                         //
// �㷨��ƣ��ܾ���                                       //
//--------------------------------------------------------//

// ȡ������ɫֵ
function TCnBitmap.GetPixel(x, y: Integer): TCnColor;
begin
  if Empty then
    raise EBitmapIsEmpty.Create(SBitmapIsEmpty);
  if (x < 0) or (x > Width - 1) or (y < 0) or (y > Height - 1) then
    raise EInvalidPixel.CreateFmt(SInvalidPixel, [x, y])
  else
    Result := FScanLine[y, x];
end;

// д����
procedure TCnBitmap.SetPixel(x, y: Integer; const Value: TCnColor);
begin
  if Empty then
    raise EBitmapIsEmpty.Create(SBitmapIsEmpty);
  if (x < 0) or (x > Width - 1) or (y < 0) or (y > Height - 1) then
    raise EInvalidPixel.CreateFmt(SInvalidPixel, [x, y])
  else
    FScanLine[y, x] := Value;
end;

// ȡɨ����
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
// ���ⲿ���ݽ���                                         //
// �㷨��ƣ��ܾ���                                       //
// �㷨�ο���Graphic32��pnBitmap                          //
//--------------------------------------------------------//

// ���ÿ�λͼ
procedure TCnBitmap.LoadBlank(AWidth, AHeight: Integer);
begin
  SetSize(AWidth, AHeight);
end;

// �ڴ���װ��λͼ��RGB ���ݿ飩
procedure TCnBitmap.LoadFromMemory(ABits: Pointer; AWidth, AHeight: Integer);
begin
  Changing;
  SetSize(AWidth, AHeight);   // ����λͼ�ߴ�
  if not Empty then
    Move(ABits^, FBits^, FSize); // ����λͼ����
  Changed;
end;

// ������װ��λͼ
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

// ���ļ���װ��λͼ��ͨ�� TPicture װ�أ�֧�� BMP��ICO��JPEG �ȸ�ʽ��
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

// ����Դ��װ��λͼ����Դ ID��
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

// ����Դ��װ��λͼ����Դ����
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

// �Ӽ�������װ��λͼ
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

// ����λͼ����
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

// ����λͼ���ļ�
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

// ����λͼ����������
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
// ͼ����ƹ���                                           //
// ԭʼ�㷨��FastLib                                      //
// �㷨�޸ģ��ܾ�������͸�����ƹ��ܡ���չ�Ͳ��������� //
//--------------------------------------------------------//

// ������λͼ�ཻλ��
function TCnBitmap.CalcDrawRect(DstX, DstY: Integer; SrcRect,
  SrcClientRect: TRect; var dx, dy, sx, sy, w, h: Integer): Boolean;
begin
  dx := DstX;
  dy := DstY;
  DeRect(SrcRect, sx, sy, w, h);
  if dx < 0 then              // Ŀ�����곬����Χ
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
  if sx < 0 then              // Դ���곬����Χ
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

// ���� TCnBitmap λͼ��ǿ��
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
  if Tran then                // ͸������
  begin
    TranColor := Src.GetTranColor;
    for I := 0 to h - 1 do
    begin
      p1 := @FScanLine[y + I][x];
      P2 := @Src.FScanLine[sy + I][sx];
      for J := 0 to w - 1 do
      begin                   // ͸���ж�
        if (p2.b <> TranColor.b) or (p2.g <> TranColor.g) or (p2.r <> TranColor.r) then
          p1^ := p2^;
        Inc(p1);
        Inc(p2);
      end;
    end;
  end
  else
  begin
    n1 := @FScanLine[y][x];   // Ŀ��λͼ���Ͻ����ص�ַ
    n2 := @Src.FScanLine[sy][sx]; // Դλͼ���Ͻ����ص�ַ
    for I := 0 to h - 1 do
    begin
      Move(n2^, n1^, w * 3);  // ����ͼ������
      n1 := Pointer(TCnNativeInt(n1) + RowInc); // ����һ��ɨ����
      n2 := Pointer(TCnNativeInt(n2) + Src.RowInc);
    end;
  end;
  Changed;
end;

// ���� TCnBitmap λͼ
procedure TCnBitmap.Draw(DstX, DstY: Integer; Src: TCnBitmap);
begin
  if Empty or not Assigned(Src) or Src.Empty then Exit;
  DrawEx(DstX, DstY, Src, Src.ClientRect);
end;

// ���� TCnBitmap λͼ��ǿ��
procedure TCnBitmap.DrawEx(DstX, DstY: Integer; Src: TCnBitmap;
  SrcRect: TRect);
begin
  if Empty or not Assigned(Src) or Src.Empty then Exit;
  DoDraw(DstX, DstY, Src, SrcRect, Src.Transparent);
end;

// ���� TGraphic λͼ��TBitmap��TIcon��TJpegImage ��
procedure TCnBitmap.Draw(DstX, DstY: Integer; Src: TGraphic);
begin
  if Empty or not Assigned(Src) or Src.Empty then Exit;
  DrawEx(DstX, DstY, Src, Rect(0, 0, Src.Width, Src.Height));
end;

// ���� TGraphic λͼ��ǿ��
procedure TCnBitmap.DrawEx(DstX, DstY: Integer; Src: TGraphic;
  SrcRect: TRect);
begin
  if Empty or not Assigned(Src) or Src.Empty then Exit;
  Changing;
  TGraphicAccess(Src).Draw(Canvas, Rect(DstX, DstY, DstX +
    SrcRect.Right - SrcRect.Left, DstY + SrcRect.Bottom - SrcRect.Top));
  if GdiAllocStyle = gsInternal then HandleRelease; //�ͷž��
  Changed;
end;

// �� DC �ϸ���λͼ������ָ��Դ����
procedure TCnBitmap.Draw(DstX, DstY: Integer; hSrc: HDC; SrcRect: TRect);
begin
  if Empty then Exit;
  Changing;
  Bitblt(DC, DstX, DstY, SrcRect.Right - SrcRect.Left, SrcRect.Bottom -
    SrcRect.Top, hSrc, SrcRect.Left, SrcRect.Top, SRCCOPY);
  if GdiAllocStyle = gsInternal then HandleRelease; //�ͷž��
  Changed;
end;

// �������� DC
procedure TCnBitmap.DrawTo(hDst: HDC; DstX, DstY: Integer);
begin
  DrawToEx(hDst, DstX, DstY, ClientRect);
end;

// �������� DC ��ǿ��
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
    if GdiAllocStyle = gsInternal then HandleRelease; //�ͷž��
  end;
end;

// ȡ�ȱ����ž���
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

// ��ָ��ģʽ����
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

// ��ָ��ģʽ���ƣ�֧�� Alpha ���
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
        Bmp := TCnBitmap.Create; // ��ʱλͼ
        try
          if Src.Transparent then
            Bmp.Assign(Self)  // ͸��ʱ��������
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
        Bmp := TCnBitmap.Create; // ��ʱλͼ
        try
          ARect := GetResizeRect(Src.ClientRect);
          Bmp.SetSize(ARect);
          if Src.Transparent then // ͸��ʱ��������
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

// ��ָ��ģʽ����
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

// ��ָ��ģʽ���ƣ�֧�� Alpha ���
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
// ͼ�����Ļ��ƹ���                                       //
// �㷨��ƣ��ܾ���                                       //
//--------------------------------------------------------//

// ���� TCnBitmap λͼ������
procedure TCnBitmap.CenterDraw(Src: TCnBitmap);
begin
  if Empty or not Assigned(Src) or Src.Empty then Exit;
  Draw((Width - Src.Width) div 2, (Height - Src.Height) div 2, Src);
end;

// ���� TGraphic ������
procedure TCnBitmap.CenterDraw(Src: TGraphic);
begin
  if Empty or not Assigned(Src) or Src.Empty then Exit;
  Draw((Width - Src.Width) div 2, (Height - Src.Height) div 2, Src);
end;

//--------------------------------------------------------//
// ͼ��ƽ�̻��ƹ���                                       //
// �㷨��ƣ��ܾ���                                       //
//--------------------------------------------------------//

// ƽ�̻��� TCnBitmap λͼ
procedure TCnBitmap.TileDraw(Src: TCnBitmap);
begin
  TileDrawEx(ClientRect, Src);
end;

// ƽ�̻��� TCnBitmap λͼ��ǿ��
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

// ƽ�̻��� TGraphic
procedure TCnBitmap.TileDraw(Src: TGraphic);
begin
  TileDrawEx(ClientRect, Src);
end;

// ƽ�̻��� TGraphic ��ǿ��
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

// ƽ�̻������� DC
procedure TCnBitmap.TileDrawTo(hDst: HDC; DstRect: TRect);
var
  I, J, x, y, w, h: Integer;
  Bmp: TCnBitmap;
begin
  if Empty then Exit;
  if Transparent then         // ͸������ʱ����һ������
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
// ͼ�����Ŵ������                                       //
// ԭʼ�㷨��FastLib                                      //
// �㷨�޸ģ��ܾ�������͸�����ƹ��ܡ���չ�Ͳ��������� //
//--------------------------------------------------------//

// ���ڽ���ֵ�㷨���ţ��ֲڣ�
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
  if (xScale < 1) or (yScale < 1) then // ��С
  begin
    xiScale := (Width shl 16) div Dst.Width;
    yiScale := (Height shl 16) div Dst.Height;
    yP := 0;
    if Tran then              // ͸��
    begin
      for y := 0 to Dst.Height - 1 do
      begin
        xP := 0;
        Read := FScanLine[yP shr 16]; // Դͼ�����ڽ���
        pc := @Dst.FScanLine[y][0];
        for x := 0 to Dst.Width - 1 do
        begin
          with Read[xP shr 16] do // ͸������
            if (b <> TranColor.b) or (g <> TranColor.g) or (r <> TranColor.r) then
              pc^ := Read[xP shr 16]; // Դͼ�����ڽ�����
          Inc(pc);
          Inc(xP, xiScale);
        end;
        Inc(yP, yiScale);
      end;
    end
    else
    begin                     // ��͸��
      for y := 0 to Dst.Height - 1 do
      begin
        xP := 0;
        Read := FScanLine[yP shr 16]; // Դͼ�����ڽ���
        pc := @Dst.FScanLine[y][0];
        for x := 0 to Dst.Width - 1 do
        begin
          pc^ := Read[xP shr 16]; // Դͼ�����ڽ�����
          Inc(pc);
          Inc(xP, xiScale);
        end;
        Inc(yP, yiScale);
      end;
    end;
  end
  else
  begin                       // �Ŵ�
    if Tran then              // ͸��
    begin
      for y := 0 to Height - 1 do
      begin
        yP := Round(yScale * y); // Ŀ��ͼ�����ڽ���ʼ��
        yD := Round(yScale * (y + 1)) - 1; // Ŀ��ͼ�����ڽ�������
        if yD > Dst.Height - 1 then yD := Dst.Height - 1;
        Read := FScanLine[y]; // Դͼ��ǰ��
        for x := 0 to Width - 1 do
        begin
          Tmp := Read[x];     // Դͼ��ǰ����
          if (Tmp.b <> TranColor.b) or (Tmp.g <> TranColor.g) or
            (Tmp.r <> TranColor.r) then // ͸������
          begin
            xP := Round(xScale * x); // Ŀ��ͼ�����ڽ���ʼ����
            xD := Round(xScale * (x + 1)) - 1; // Ŀ��ͼ�����ڽ���������
            if xD > Dst.Width - 1 then xD := Dst.Width - 1;
            for xCount := xP to xD do
              Dst.FScanLine[yP][xCount] := Tmp; // ����һ��
            for yCount := yP + 1 to yD do
              Move(Dst.FScanLine[yP][xP], Dst.FScanLine[yCount][xP],
                (xD - xP + 1) * 3); //���Ƶ�������
          end;
        end;
      end;
    end
    else
    begin                     // ��͸��
      yiScale := Round(yScale + 0.5); // �Ŵ�������ֵ��
      xiScale := Round(xScale + 0.5);
      GetMem(Line, Dst.Width * 3); // ��ʱɨ����
      for y := 0 to Height - 1 do
      begin
        yP := Round(yScale * y); // Ŀ��ͼ�����ڽ���ʼ��
        Read := FScanLine[y];    // Դͼ��ǰ��
        for x := 0 to Width - 1 do
        begin
          xP := Round(xScale * x); // Ŀ��ͼ�����ڽ���ʼ����
          Tmp := Read[x];     // Դͼ��ǰ����
          for xCount := 0 to xiScale - 1 do
          begin
            xD := xCount + xP;
            if xD >= Dst.Width then Break;
            Line[xD] := Tmp;
          end;
        end;
        for yCount := 0 to yiScale - 1 do // ���Ƶ��ڽ���
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

// ���ٶ��β�ֵ�㷨���ţ�ƽ����
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
  xP2 := ((Width - 1) shl 15) div Dst.Width; // ���ű���
  yP2 := ((Height - 1) shl 15) div Dst.Height;
  yP := 0;
  for y := 0 to Dst.Height - 1 do
  begin
    pc := @Dst.FScanLine[y][0]; // Ŀ��ɨ����
    Read := FScanLine[yP shr 15]; // Դ��ɨ����
    Read2 := FScanLine[yP shr 15 + 1]; // Դ��ɨ����
    z2 := yP and $7FFF;       // Դ����������ɨ����֮�� "y"
    iz2 := $8000 - z2;        // Դ����������ɨ����֮�� "1-y"
    xP := 0;
    for x := 0 to Dst.Width - 1 do
    begin
      t := xP shr 15;
      z := xP and $7FFF;      // Դ����������������֮�� "x"
      Col1 := @Read[t];       // �������� "f(0,0)"
      Col2 := @Read[t + 1];   // �������� "f(1,0)"
      Col3 := @Read2[t];      // �������� "f(0,1)"
      Col4 := @Read2[t + 1];  // �������� "f(1,1)"
      if Tran then
        with TranColor do
        begin                 // ͸��ʱȡĿ������
          if (Col1.b = b) and (Col1.g = g) and (Col1.r = r) then Col1 := pc;
          if (Col2.b = b) and (Col2.g = g) and (Col2.r = r) then Col2 := pc;
          if (Col3.b = b) and (Col3.g = g) and (Col3.r = r) then Col3 := pc;
          if (Col4.b = b) and (Col4.g = g) and (Col4.r = r) then Col4 := pc;
        end;                  // ������һ�㲻͸��
      if (Col1 <> pc) or (Col2 <> pc) or (Col3 <> pc) or (Col4 <> pc) then
      begin
        // �����Ȩֵ
        w2 := (z * iz2) shr 15; // ���� p(1,0) = x(1-y);
        w1 := iz2 - w2;         // ���� p(0,0) = (1-y)(1-x) = (1-y)-p(0,1)
        w4 := (z * z2) shr 15;  // ���� p(1,1) = x*y
        w3 := z2 - w4;          // ���� p(0,1) = y(1-x) = y-p(1,1)
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
// ͼ�����Ż��ƹ���                                       //
// �㷨��ƣ��ܾ���                                       //
//--------------------------------------------------------//

// ���Ż��� TCnBitmap λͼ
procedure TCnBitmap.StretchDraw(Src: TCnBitmap);
begin
  if Empty or not Assigned(Src) or Src.Empty then Exit;
  BeginUpdate;
  try
    if (Src.Width = Width) and (Src.Height = Height) then
    begin
      Draw(0, 0, Src);        // �ߴ����
      Exit;
    end;
    if SmoothFilter then      // ƽ������
      Src.SmoothResize(Self)  // ���β�ֵ�㷨
    else
      Src.NormalResize(Self); // ���ڽ���ֵ�㷨
  finally
    EndUpdate;
  end;
end;

// ���Ż��� TCnBitmap λͼ��ǿ��
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
        SrcBmp := TCnBitmap.Create; // Դλͼ�е�Դ��������
        SrcBmp.SetSize(SrcRect);
        SrcBmp.Transparent := Src.Transparent;
        SrcBmp.TransparentColor := Src.TransparentColor;
        SrcBmp.DoDraw(0, 0, Src, SrcRect, False); // ��͸������
      end;
      if RectEqu(DstRect, ClientRect) then
        StretchDraw(SrcBmp)
      else
      begin
        DstBmp := TCnBitmap.Create; // Ŀ��λͼ��Ŀ���������
        try
          DeRect(DstRect, x, y, w, h);
          DstBmp.SetSize(w, h);
          if SrcBmp.Transparent then // ͸��ʱ��������
            DstBmp.DoDraw(0, 0, Self, DstRect, False);
          DstBmp.SmoothFilter := SmoothFilter; // ���ŷ�ʽ
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

// ���Ż��� TGraphic
procedure TCnBitmap.StretchDraw(Src: TGraphic);
begin
  StretchDrawEx(ClientRect, Rect(0, 0, Src.Width, Src.Height), Src);
end;

// ���Ż��� TGraphic ��ǿ��
procedure TCnBitmap.StretchDrawEx(DstRect, SrcRect: TRect; Src: TGraphic);
var
  SrcBmp: TCnBitmap;
begin
  if Empty or not Assigned(Src) or Src.Empty then Exit;
  BeginUpdate;
  try
    SrcBmp := TCnBitmap.Create; // Դ����
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

// ���Ż��� DC
procedure TCnBitmap.StretchDraw(SrcRect: TRect; hSrc: HDC);
var
  SrcBmp: TCnBitmap;
begin
  if Empty then Exit;
  BeginUpdate;
  try
    SrcBmp := TCnBitmap.Create; // Դ����
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

// ���Ż��� DC ��ǿ��
procedure TCnBitmap.StretchDrawEx(DstRect, SrcRect: TRect; hSrc: HDC);
var
  DstBmp: TCnBitmap;
  x, y, w, h: Integer;
begin
  if Empty then Exit;
  BeginUpdate;
  try
    DstBmp := TCnBitmap.Create; // Ŀ�����
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

// �������Ż��Ƶ� TImage
procedure TCnBitmap.StretchDrawTo(Dst: TImage);
begin
  if Assigned(Dst) then
  begin
    StretchDrawTo(Dst.Canvas.Handle, Dst.ClientRect);
    Dst.Refresh;
  end;
end;

// �������Ż��Ƶ� DC
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

// �������Ż��Ƶ� DC ��ǿ��
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
// Alpha ��ϻ���                                         //
// �㷨��Դ��FastLib��pnBitmap                            //
// �㷨�Ľ����ܾ�������͸�����ƹ��ܡ���ǿ���ܼ��Ľ���   //
//--------------------------------------------------------//

// ���Դͼ���Ƿ���Ҫ����
function TCnBitmap.CheckAlphaSrc(Src: TCnBitmap; ARect: TRect;
  Stretch: Boolean): TCnBitmap;
begin
  if (RectWidth(ARect) <> Src.Width) or (RectHeight(ARect) <> Src.Height) then
  begin                       // ��Ҫ����
    if not Stretch then       // ���ڻ�ϵ�λͼ��С�������
      raise ECnGraphics.Create(SInvalidAlphaBitmap)
    else
    begin                     // ������ʱλͼ
      Result := TCnBitmap.Create;
      Result.SetSize(ARect);
      Result.SmoothFilter := SmoothFilter;
      if Src.Transparent then // ͸��ʱ�Ȼ�������
        Result.DoDraw(0, 0, Self, ARect, False);
      Result.StretchDraw(Src);
    end;
  end
  else
    Result := Src;
end;

// Alpha ��ϻ���
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
    Bmp := CheckAlphaSrc(Src, ClientRect, Stretch); // ����Դλͼ����
    if FAlpha = 255 then
    begin
      Draw(0, 0, Bmp);
      Exit;
    end;

    for I := -255 to 255 do   // ����Alpha��ϱ�
      Table[I] := (FAlpha * I) shr 8;

    c1 := Bits;
    c2 := Bmp.Bits;
    if (Bmp = Src) and Src.Transparent then  // Դλͼ͸����δ���ţ�����ʱ�Ѵ����͸���ˣ�
    begin
      TranColor := Src.GetTranColor; // ���ʱ͸������
      for y := 0 to FHeight - 1 do
      begin
        for x := 0 to FWidth - 1 do
        begin
          if (TranColor.b <> c2.b) or (TranColor.g <> c2.g) or (TranColor.r <> c2.r)
            then
          begin
            c1.b := Table[c2.b - c1.b] + c1.b; // ������ٻ��
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
          c1.b := Table[c2.b - c1.b] + c1.b; // ������ٻ��
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
    if Assigned(Bmp) and (Bmp <> Src) then Bmp.Free; // �ͷ���ʱλͼ
    EndUpdate;
  end;
end;

// Alpha ��ϻ��ƣ�����ָ��λ�ã�
// �㷨��ƣ��ܾ���
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
  for I := -255 to 255 do     // ���� Alpha ��ϱ�
    Table[I] := (FAlpha * I) shr 8;
  Tran := Src.Transparent;
  TranColor := Src.GetTranColor;
  for I := 0 to h - 1 do
  begin
    p1 := @FScanLine[y + I][x];
    P2 := @Src.FScanLine[sy + I][sx];
    for J := 0 to w - 1 do
    begin                     // ͸���ж�
      if not Tran or (p2.b <> TranColor.b) or (p2.g <> TranColor.g) or (p2.r <>
        TranColor.r) then
      begin
        p1.b := Table[p2.b - p1.b] + p1.b; // ������ٻ��
        p1.g := Table[p2.g - p1.g] + p1.g;
        p1.r := Table[p2.r - p1.r] + p1.r;
      end;
      Inc(p1);
      Inc(p2);
    end;
  end;
  Changed;
end;

// ����͸���� Alpha ��ϲ���
// �㷨��ƣ��ܾ���
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
    Bmp := CheckAlphaSrc(Src, ClientRect, Stretch); // ����Դλͼ����
    Tran := (Bmp = Src) and Src.Transparent; // Դλͼ͸����δ����
    TranColor := Src.GetTranColor;
    // ���㽥�� Alpha ��
    if Style in [gsLeftToRight, gsRightToLeft, gsCenterToLR] then
      BufLen := FWidth        // ����������
    else if Style in [gsTopToBottom, gsBottomToTop, gsCenterToTB] then
      BufLen := FHeight
    else if Style = gsRadial then
      BufLen := Max(FWidth, FHeight)
    else
      Exit;
    if Style in [gsCenterToLR, gsCenterToTB] then
      Len := (BufLen + 1) div 2 // ���������
    else
      Len := BufLen;

    GetMem(Alpha, BufLen);
    if Style in [gsLeftToRight, gsTopToBottom, gsRadial] then
    begin
      SA := AlphaToInt(StartAlpha) shl 16; // ���򽥱�
      EA := AlphaToInt(EndAlpha) shl 16;
    end else
    begin
      SA := AlphaToInt(EndAlpha) shl 16;
      EA := AlphaToInt(StartAlpha) shl 16;
    end;
    AddA := Round((EA - SA) / Len); // ÿ��������
    CurA := SA;
    for I := 0 to Len - 1 do
    begin
      Alpha[I] := CurA shr 16; // С��ת�����Ż�
      Inc(CurA, AddA);
    end;

    if Style in [gsCenterToLR, gsCenterToTB] then // �Գƽ���
      for I := 0 to Len - 1 do
        Alpha[BufLen - 1 - I] := Alpha[I];

    c1 := Bits;
    c2 := Bmp.Bits;
    if Style in [gsLeftToRight, gsRightToLeft, gsCenterToLR] then
    begin                     // ˮƽ���򽥱�
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
    begin                     // ��ֱ���򽥱�
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
    begin                     // ���佥��
      ta := FWidth / 2;       // ��Բ����
      tb := FHeight / 2;      // ��Բ����
      tab := ta * tb;
      for y := 0 to FHeight - 1 do
      begin
        oy := Abs(y - tb);    // ��ֱ���ľ�
        for x := 0 to FWidth - 1 do
        begin
          ox := Abs(x - ta);  // ˮƽ���ľ�
          if ox = 0 then
            Rate := oy / tb
          else if oy = 0 then
            Rate := ox / ta
          else                // ���㵱ǰ�����ľ���÷���뾶֮��
            Rate := ox * Hypot(tb, ta * oy / ox) / tab;
          if Rate >= 1 then
            Weight := Alpha[BufLen - 1]
          else                // ��ǰ�㽥��Alphaֵ
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
    if Assigned(Bmp) and (Bmp <> Src) then Bmp.Free; // �ͷ���ʱλͼ
    if Alpha <> nil then FreeMem(Alpha);
    EndUpdate;
  end;
end;

// ��ǿ�� Alpha ��ϲ�����ǰ����������ϵ�����
// �㷨��ƣ��ܾ���
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
    if xd < 0 then            // Ŀ�����곬����Χ
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

    BmpFt := CheckAlphaSrc(Front, DstRect, Stretch); // ����Դλͼ����
    BmpBk := CheckAlphaSrc(Back, DstRect, Stretch);
    TranFt := (BmpFt = Front) and Front.Transparent;
    TranColorFt := Front.GetTranColor;
    TranBk := (BmpBk = Back) and Back.Transparent;
    TranColorBk := Back.GetTranColor;

    FAlpha := AlphaToInt(Alpha);
    for I := -255 to 255 do   // ���� Alpha ��ϱ�
      Table[I] := (FAlpha * I) shr 8;

    if TranFt or TranBk then  // ��Ҫ����͸�����ٶ��½�Լ 20%��
    begin
      for y := 0 to h - 1 do
      begin
        c1 := @FScanLine[y + yd][xd];
        c2 := @BmpFt.FScanLine[y + ys][xs];
        c3 := @BmpBk.FScanLine[y + ys][xs];
        for x := 0 to w - 1 do
        begin
          if not TranFt or (TranColorFt.b <> c2.b) or (TranColorFt.g <> c2.g)
            or (TranColorFt.r <> c2.r) then // ǰ��ͼ͸���ж�
            cFt := c2
          else
            cFt := c1;
          if not TranBk or (TranColorBk.b <> c2.b) or (TranColorBk.g <> c2.g)
            or (TranColorBk.r <> c2.r) then // ����ͼ͸���ж�
            cBk := c3
          else
            cBk := c1;
          c1.b := Table[cFt.b - cBk.b] + cBk.b; // ������ٻ��
          c1.g := Table[cFt.g - cBk.g] + cBk.g;
          c1.r := Table[cFt.r - cBk.r] + cBk.r;
          Inc(c1);
          Inc(c2);
          Inc(c3);
        end;
      end;
    end
    else                      // ����Ҫ����͸��
    begin
      for y := 0 to h - 1 do
      begin
        c1 := @FScanLine[y + yd][xd];
        c2 := @BmpFt.FScanLine[y + ys][xs];
        c3 := @BmpBk.FScanLine[y + ys][xs];
        for x := 0 to w - 1 do
        begin
          c1.b := Table[c2.b - c3.b] + c3.b; // ������ٻ��
          c1.g := Table[c2.g - c3.g] + c3.g;
          c1.r := Table[c2.r - c3.r] + c3.r;
          Inc(c1);
          Inc(c2);
          Inc(c3);
        end;
      end;
    end;
  finally
    if Assigned(BmpFt) and (BmpFt <> Front) then BmpFt.Free; // �ͷ���ʱλͼ
    if Assigned(BmpBk) and (BmpBk <> Back) then BmpBk.Free;
    EndUpdate;
  end;
end;

//--------------------------------------------------------//
// ������ɫ����                                           //
// �㷨��ƣ��ܾ���                                       //
//--------------------------------------------------------//

// ���ƽ���ɫ
procedure TCnBitmap.DrawGradient(GradColor: TCnGradientColor);
begin
  DrawGradientEx(GradColor, ClientRect, csMaxAlpha);
end;

// ���ƽ���ɫ��ǿ��
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
      for I := -255 to 255 do // ���� Alpha ��ϱ�
        Table[I] := (FAlpha * I) shr 8;

    if GradColor.FStyle in [gsLeftToRight, gsRightToLeft, gsCenterToLR] then
      BufLen := RectWidth(Rect) // ����������
    else if GradColor.FStyle in [gsTopToBottom, gsBottomToTop, gsCenterToTB] then
      BufLen := RectHeight(Rect)
    else if GradColor.FStyle = gsRadial then
      BufLen := Max(RectWidth(Rect), RectHeight(Rect))
    else
      Exit;
    if GradColor.FStyle in [gsCenterToLR, gsCenterToTB] then
      Len := (BufLen + 1) div 2 // ���������
    else
      Len := BufLen;
    BufSize := BufLen * 3;
    GetMem(Buf, BufSize);
    try
      // ��������ɫ��������
      IsInvert := GradColor.FStyle in [gsRightToLeft, gsBottomToTop,
        gsCenterToLR, gsCenterToTB]; // ����
      I := 0;
      SCol := CnColor(GradColor.FColorStart); // ��ʼɫ
      SPos := 0;              // ��ʼλ��
      repeat
        if Assigned(GradColor.FColorMiddle) and (I < GradColor.FColorMiddle.Count) then
        begin                 // ���м�ɫ�в���
          ECol := CnColor(GradColor.FColorMiddle[I].FColor);
          EPos := GradColor.FColorMiddle[I].FPos;
          Inc(I);
        end
        else
        begin
          ECol := CnColor(GradColor.FColorEnd); // ����ɫ
          EPos := csMaxGradPos;
        end;
        Head := SPos * Len div csMaxGradPos; // ��ʼλ�ü�����
        Added := Min((EPos - SPos) * Len div csMaxGradPos + 1, Len - Head);
        for J := 0 to Added - 1 do
        begin
          if IsInvert then    // ��鷴��
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

      if GradColor.FStyle in [gsCenterToLR, gsCenterToTB] then // �Գƽ���
        for I := 0 to Len - 1 do
          Buf[BufLen - 1 - I] := Buf[I];

      OffX := ARect.Left - Rect.Left;
      OffY := ARect.Top - Rect.Top;
      if GradColor.FStyle in [gsLeftToRight, gsRightToLeft, gsCenterToLR] then
      begin                   // ˮƽ����
        if FAlpha = 255 then  // ��͸��
        begin
          LineSize := RectWidth(ARect) * 3;
          for I := ARect.Top to ARect.Bottom - 1 do
            Move(Buf[OffX], FScanLine[I][ARect.Left], LineSize);
        end
        else
        begin                 // ͸����
          for y := ARect.Top to ARect.Bottom - 1 do
          begin
            Col := @FScanLine[y][ARect.Left];
            Col1 := @Buf[OffX];
            for x := 0 to RectWidth(ARect) - 1 do
            begin             // ������ٻ��
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
      begin                   // ��ֱ����
        if FAlpha = 255 then  // ��͸��
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
        begin                 // ͸����
          for y := 0 to RectHeight(ARect) - 1 do
          begin               // ������ٻ��
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
      begin                   // ���佥��
        ta := RectWidth(Rect) / 2; // ��Բ����
        tb := RectHeight(Rect) / 2; // ��Բ����
        ta2 := Sqr(ta);
        tab := ta * tb;
        tab2 := Sqr(tab);
        cx := (Rect.Left + Rect.Right - 1) / 2; // ���ĵ�
        cy := (Rect.Top + Rect.Bottom - 1) / 2;
        for y := ARect.Top to ARect.Bottom - 1 do
        begin
          oy := Abs(y - cy);  // ��ֱ���ľ�
          if FSmoothFilter then
            lx := Sqrt(tab2 - ta2 * Sqr(oy - 1)) / tb // ���ǿ���ݴ���
          else                // ������Ч���ƿ��
            lx := Sqrt(tab2 - ta2 * Sqr(oy)) / tb;
          XL := Max(Floor(cx - lx), ARect.Left); // ��Բ��߽�
          XR := Min(Ceil(cx + lx), ARect.Right - 1); // �ұ߽�
          Col := @FScanLine[y][XL];
          for x := XL to XR do
          begin
            ox := Abs(x - cx); // ˮƽ���ľ�
            if ox = 0 then
              Rate := oy / tb
            else if oy = 0 then
              Rate := ox / ta
            else              // ���㵱ǰ�����ľ���÷���뾶֮��
              Rate := ox * Hypot(tb, ta * oy / ox) / tab;
            if Rate <= 1 then
            begin             // ��ǰ�㽥����ɫֵ
              Col1 := @Buf[Round(Rate * (BufLen - 1))];
              if FAlpha = 255 then // ��͸��
                Col^ := Col1^
              else
              begin           // ������ٻ��
                Col.b := Table[Col1.b - Col.b] + Col.b;
                Col.g := Table[Col1.g - Col.g] + Col.g;
                Col.r := Table[Col1.r - Col.r] + Col.r;
              end;
            end
            else if FSmoothFilter then // �߽�㿹��ݴ���
            begin
              BdWeight := Round((1 - (Rate - 1) * (BufLen - 1)) * FAlpha);
              if BdWeight > 0 then // ��͸����
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
// ��ťλͼ���ƹ���                                       //
// �㷨��ƣ��ܾ���                                       //
//           ��Rxlib���㷨��һ������                      //
// �㷨�ο���Rxlib��pnBitmap                              //
//--------------------------------------------------------//

// ʧЧλͼ
procedure TCnBitmap.Disabled;
begin
  DisabledEx(CnWinColor(GetTranColor), clBtnface, clBtnHighlight, clBtnShadow, True);
end;

// ʧЧλͼ��ǿ��
procedure TCnBitmap.DisabledEx(OutlineColor, BackColor, HighlightColor,
  ShadowColor: TColor; DrawHighlight: Boolean);
var
  BmpLight: TCnBitmap;
begin
  if Empty then Exit;
  BeginUpdate;
  try
    MaskEx(OutlineColor, BackColor, ShadowColor); // �������ɫ��
    TransparentColor := BackColor; // ͸��ɫ��Ϊ����ɫ
    if DrawHighlight then     // �����½ǻ��Ƹ���ͼ
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

// ����ʧЧλͼ
procedure TCnBitmap.DrawDisabled(hDst: HDC; ARect: TRect);
begin
  DrawDisabledEx(hDst, ARect, CnWinColor(GetTranColor), clBtnface,
    clBtnHighlight, clBtnShadow, True);
end;

// ����ʧЧλͼ��ǿ��
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

// ��Ӱλͼ
procedure TCnBitmap.Shadowed;
begin
  ShadowedEx(CnWinColor(GetTranColor), clBlack, clBtnface, True, 2, 2);
end;

// ��Ӱλͼ��ǿ��
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
      Bmp.SetSize(ClientRect); // ��ʱλͼ
      Bmp.Fill(BackColor);
      TransparentColor := OutlineColor; // ͸�����Ƶ���ʱλͼ
      Bmp.DoDraw(-OffsetX, -OffsetY, Self, ClientRect, True);
      Bmp.Transparent := True;
      Bmp.TransparentColor := BackColor; // �������ɫ��
      MaskEx(OutlineColor, BackColor, ShadowColor);
      TransparentColor := BackColor;
      if Blur then Self.Blur; // ��Ӱģ��
      DoDraw(0, 0, Bmp, ClientRect, True); // ����ʱλͼ����
    finally
      Bmp.Free;
    end;
  finally
    EndUpdate;
  end;
end;

// ������Ӱλͼ
procedure TCnBitmap.DrawShadowed(hDst: HDC; ARect: TRect);
begin
  DrawShadowedEx(hDst, ARect, CnWinColor(GetTranColor), clBlack, clBtnface,
    True, 2, 2);
end;

// ������Ӱλͼ��ǿ��
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
// ͼ����ɫ���Ե���                                       //
// �㷨��Դ��FastLib��pnBitmap                            //
// �㷨�޸ģ��ܾ���                                       //
//--------------------------------------------------------//

// ��������ɫ����
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

// ����ͼ������
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
  if Amount > 0 then          // ����
    for x := 0 to 255 do
      Table[x] := IntToByte(x + ((Amount * (x xor 255)) shr 8))
  else
  begin
    Amount := -Amount;        // �䰵
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

// ����ͼ�񱥺Ͷ�
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
      begin                   // ���
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
      begin                   // ���
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

// ����ͼ��Աȶ�
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

// ������ǰͼ���ɫ��
// �㷨��ƣ��ܾ���
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

// ͼ��ҶȻ�
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

// ��ɫ��ת
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
        CurBits.b := not CurBits.b; // ��ɫȡ��
        CurBits.g := not CurBits.g; // һ�����ָ�����
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
        if ccBlue in Channels then CurBits.b := not CurBits.b; // ��ɫȡ��
        if ccGreen in Channels then CurBits.g := not CurBits.g; // һ�����ָ�����
        if ccRed in Channels then CurBits.r := not CurBits.r;
        Inc(CurBits);
      end;
      CurBits := Pointer(TCnNativeInt(CurBits) + Gap);
    end;
  end;
  Changed;
end;

// ͼ��ָ����ɫ��ɫ����֧��͸����ʽ��
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
// ͼ�񼸺α任                                           //
// �㷨��Դ��FastLib��pnBitmap���ܾ���                    //
// �㷨�޸ģ��ܾ���                                       //
//--------------------------------------------------------//

// ͼ��ֱ��ˮƽ����ת
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
      if Horizontal then      // ˮƽ��ת
        for x := 0 to w div 2 do
        begin
          CurBits := Line[x];
          Line[x] := Line[w - x];
          Line[w - x] := CurBits;
        end
      else
      begin                   // ��ֱ��ת
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

// ͼ����ת
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

// ˮƽ�ƶ�ͼ��
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
  GetMem(Line, Amount * 3);   // ��ʱ������
  try
    p := Bits;
    for y := 0 to Height - 1 do
    begin                     // ������벿�ֵ���ʱ������
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

// ��ֱ�ƶ�ͼ��
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
  GetMem(Buff, y);            // ��ʱ������

  try
    CopyMemory(Buff, FScanLine[Height - Amount], y);
    MoveMemory(FScanLine[Amount], Bits, p);
    CopyMemory(Bits, Buff, y);
  finally
    FreeMem(Buff);
  end;
  Changed;
end;

// λͼ��ת
// �㷨��ƣ��ܾ��� 2002.01.27
// ֧�����ڽ���ֵ�㷨�Ϳ��ٶ��β�ֵ�㷨��SmoothFilter ���ԣ�
// ֧��������͸����ʽ
// Angle: �Ƕ� -360..360;
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
  FAngle := -Angle * Pi / 180; // ���������ԭʼ��
  iSin := Round(Sin(FAngle) * $8000);
  iCos := Round(Cos(FAngle) * $8000);
  for y := ARect.Top to ARect.Bottom - 1 do
  begin
    py := y - DstCenter.y;
    for x := ARect.Left to ARect.Right - 1 do
    begin
      px := x - DstCenter.x;
      SrcX := px * iCos - py * iSin + Src.Width shl 14; // ԭʼ��
      SrcY := px * iSin + py * iCos + Src.Height shl 14;
      if not SmoothFilter then
      begin                   // ���ڽ���ֵ�㷨
        xs := SrcX + $3FFF;
        ys := SrcY + $3FFF;
        if (xs >= 0) and (ys >= 0) then
        begin
          xs := xs shr 15;
          ys := ys shr 15;
          if (xs < Src.Width) and (ys < Src.Height) then // ��Χ���
          begin
            Col1 := @Src.FScanLine[ys][xs];
            if not Tran or (TranColor.b <> Col1.b) or (TranColor.g <> Col1.g) or
              (TranColor.r <> Col1.r) then // ͸�����
              FScanLine[y][x] := Col1^;
          end
        end
      end
      else
      begin                   // ���β�ֵ�㷨
        if SrcX > 0 then
          x1 := SrcX shr 15   // ���ڽ�����
        else
          x1 := -(-SrcX shr 15);
        x2 := x1 + 1;         // ���ڽ�����
        if SrcY > 0 then
          y1 := SrcY shr 15
        else
          y1 := -(-SrcY shr 15); // ���ڽ�ɨ����
        y2 := y1 + 1;         // ���ڽ�ɨ����
        if (x2 >= 0) and (x1 < Src.Width) and (y2 >= 0) and
          (y1 < Src.Height) then // ��Χ���
        begin
          Dst := @FScanLine[y][x];
          if (x1 >= 0) and (y1 >= 0) then
          begin
            Col1 := @Src.FScanLine[y1][x1]; // ���Ͻ�����
            if Tran and (TranColor.b = Col1.b) and (TranColor.g = Col1.g) and
              (TranColor.r = Col1.r) then // ͸�����
              Col1 := Dst;
          end
          else
            Col1 := Dst;
          if (x2 < Src.Width) and (y1 >= 0) then // ���Ͻ�����
          begin
            Col2 := @Src.FScanLine[y1][x2]; // ͸�����
            if Tran and (TranColor.b = Col2.b) and (TranColor.g = Col2.g) and
              (TranColor.r = Col2.r) then
              Col2 := Dst;
          end
          else
            Col2 := Dst;
          if (x1 >= 0) and (y2 < Src.Height) then // ���½�����
          begin
            Col3 := @Src.FScanLine[y2][x1]; // ͸�����
            if Tran and (TranColor.b = Col3.b) and (TranColor.g = Col3.g) and
              (TranColor.r = Col3.r) then
              Col3 := Dst;
          end
          else
            Col3 := Dst;
          if (x2 < Src.Width) and (y2 < Src.Height) then // ���½�����
          begin
            Col4 := @Src.FScanLine[y2][x2];
            if Tran and (TranColor.b = Col4.b) and (TranColor.g = Col4.g) and
              (TranColor.r = Col4.r) then // ͸�����
              Col4 := Dst;
          end
          else
            Col4 := Dst;
          if (Col1 <> Dst) or (Col2 <> Dst) or (Col3 <> Dst) or (Col4 <> Dst) then
          begin               // ������һ�㲻͸��
            zx := SrcX and $7FFF;
            zy := SrcY and $7FFF;
            izy := zy xor $7FFF;
            w2 := (zx * izy) shr 15; // �����Ȩֵ
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
// ͼ���˾�����                                           //
// �㷨��Դ��FastLib��pnBitmap                            //
// �㷨�޸ģ��ܾ���                                       //
//--------------------------------------------------------//

// 3X3�������
// �㷨�޸ģ��ܾ���ʹ�����ݻ����������ٶ��Ż�Լһ����
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
    Dst := Bits;              // Ŀ������
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

// ģ��������ͨ�˲���
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

// ����ģ��
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

// ���ٸ�˹ģ��
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

// �񻯴�����ͨ�˲���
procedure TCnBitmap.Sharpen;
begin
  SplitSharpen(1);
end;

// ������
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
      Buf[6] := Lin0[cx];     // ����ˣ�-1/8 -1/8 -1/8
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

// ��ǿ��
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

// �罦Ч��
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

// ����Ч��
procedure TCnBitmap.Emboss;
var
  x, y: Integer;
  p1, p2: PCnColor;
  Line: PPCnLines;
begin
  if Empty then Exit;
  Changing;
  p1 := Bits;                 // ��һ��
  p2 := Pointer(TCnNativeInt(p1) + RowInc + 3); // ���¸���һ����
  GetMem(Line, RowInc);       // ��ʱ�б������һ��ɨ��������
  try
    CopyMemory(Line, FScanLine[FHeight - 1], RowInc);
    for y := 0 to Height - 1 do
    begin
      for x := 0 to Width - 1 do
      begin
        p1.b := (p1.b + not p2.b) shr 1; // ��ǰ���������½�����ȡ����ƽ��ֵ
        p1.g := (p1.g + not p2.g) shr 1;
        p1.r := (p1.r + not p2.r) shr 1;
        Inc(p1);
        if (y < FHeight - 2) and (x < FWidth - 2) then Inc(p2);
      end;
      p1 := Pointer(TCnNativeInt(p1) + FGap);
      if y < FHeight - 2 then // �������
        p2 := Pointer(TCnNativeInt(p2) + Gap + 6)
      else
        p2 := Pointer(TCnNativeInt(Line) + 3);
    end;
  finally
    FreeMem(Line);
  end;
  Changed;
end;

// �ڻ�Ч��
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

// ��ͼЧ��
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

// ˮ��Ч��
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

// ����Ч��
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

// �����˻�
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

// ����Ч��
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

// �ڵ�ǰͼ���ϲ�������Ч��
// �㷨��ƣ��ܾ��� 2002.03.04
// ʹ���Ż��㷨���ٶȽϿ�
procedure TCnBitmap.Lighting(Center: TPoint; OffX, OffY: Integer;
  Angle: Double; Color: TColor; Amount: TCnAlpha);
var
  Col: PCnColor;
  FAlpha: Integer;
  ARect: TRect;
  Table: array[0..90] of Integer; // ������Բ��Χ90���ڰ뾶����
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
  Table[0] := OffX;           // 0 ��ʱΪ����
  Table[90] := OffY;          // 90 ��ʱΪ����
  for I := 1 to 89 do
  begin
    tz := Tan(I * PI / 180);
    tx := Sqrt(tab / (tb + ta * Sqr(tz))); // i ��ʱ X ����
    ty := tx * tz;            // i ��ʱ Y ����
    Table[I] := Round(Hypot(tx, ty)); // ���ľࣨi �Ȱ뾶��
  end;

  DeRGB(Color, r, g, b);
  MLen := Max(OffX, OffY);
  for y := ARect.Top to ARect.Bottom - 1 do
  begin
    Col := @FScanLine[y, ARect.Left];
    for x := ARect.Left to ARect.Right - 1 do
    begin
      Len := Round(Hypot(x - Center.x, y - Center.y)); // ���ľ�
      if Len < MLen then
      begin
        if Center.x = x then  // ��ֱ����
          beta := 90
        else if Center.y = y then // ˮƽ����
          beta := 0
        else
          beta := Round(ArcTan((y - Center.y) / (x - Center.x)) * 180 / PI);
        beta := Round(beta - Angle) mod 360; // ��������������X��ļн�
        if beta < 0 then Inc(beta, 360);
        if beta > 270 then
          beta := 360 - beta
        else if beta > 180 then
          beta := beta - 180
        else if beta > 90 then
          beta := 180 - beta; // �任�� 90 �ȷ�Χ��
        if Len <= Table[beta] then
        begin                 // ��ȨֵΪ�㵽���ĵľ��������ĵ��÷�����Բ�ܾ���֮��
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

// ����Ч��
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

// ת��Ϊ�ڰ�����ͼ
procedure TCnBitmap.Mask(MaskColor: TCnColor);
begin
  MaskEx(MaskColor, CnColor(0, 0, 0), CnColor(255, 255, 255));
end;

procedure TCnBitmap.Mask(MaskColor: TColor);
begin
  Mask(CnColor(MaskColor));
end;

// ת��Ϊ��ɫ����ͼ
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

// ���Ӳ�ɫ������
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
    begin                     // �������ɫ��
      pc.b := IntToByte(pc.b + (Random(Amount) - (Amount shr 1)));
      pc.g := IntToByte(pc.g + (Random(Amount) - (Amount shr 1)));
      pc.r := IntToByte(pc.r + (Random(Amount) - (Amount shr 1)));
      Inc(pc);
    end;
    pc := Pointer(TCnNativeInt(pc) + Gap);
  end;
  Changed;
end;

// ���Ӻڰ�������
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
    begin                     // ��������Ҷȵ�
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

// ��ȥ�����㣨��ֵƽ������ 3x3 �����
// �㷨��ƣ��ܾ���
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
      begin                   // �ڽ�������ƽ��ֵ
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

// �����ɰ�ɫ
procedure TCnBitmap.AddMiddleColor(Color: TColor);
begin
  AddMiddleColorEx(Color, ClientRect);
end;

// �����ɰ�ɫ��ָ������
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
      pc.b := (pc.b + b) shr 1; // ��ɫƽ��ֵ
      pc.g := (pc.g + g) shr 1;
      pc.r := (pc.r + r) shr 1;
      Inc(pc);
    end;
  end;
  Changed;
end;

//--------------------------------------------------------//
// ����ͼ����                                           //
// �㷨��Դ��FastLib                                      //
// �㷨�޸ģ��ܾ���                                       //
//--------------------------------------------------------//

// �����Ľ���ɫֵ�ý���ɫ������
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
    rp := xx shl 20;          // ��ʼֵ����ֱ�����ѽ��䣩
    rp2 := (((c10.r * iz + c11.r * z) shr 20) - xx) * t; // ˮƽ����
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
// ����ݻ��ʻ��Ʒ�����֧��С����                         //
// �㷨��ƣ��ܾ���                                       //
// �㷨�ο���Graphic32��FastLib                           //
//--------------------------------------------------------//

// ȡ������������ɫ
function TCnBitmap.GetPixelsF(x, y: Single): TCnColor;
begin
  if Empty then
    raise EBitmapIsEmpty.Create(SBitmapIsEmpty);
  if (x < 0) or (x > Width - 1) or (y < 0) or (y > Height - 1) then
    raise EInvalidPixel.CreateFmt(SInvalidPixelF, [x, y])
  else
    Result := DoGetPixelF(Round(x * $8000), Round(y * $8000));
end;

// д������������ɫ
procedure TCnBitmap.SetPixelsF(x, y: Single; const Value: TCnColor);
begin
  if Empty then
    raise EBitmapIsEmpty.Create(SBitmapIsEmpty);
  if (x < 0) or (x > Width - 1) or (y < 0) or (y > Height - 1) then
    raise EInvalidPixel.CreateFmt(SInvalidPixelF, [x, y])
  else
    DoSetPixelF(Round(x * $8000), Round(y * $8000), Value);
end;

// ȡС������ $7FFF��������
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
  w2 := (zx * izy) shr 15;    // �����Ȩֵ
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

// дС������ $7FFF��������
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
  w2 := (zx * izy) shr 15;    // �����Ȩֵ
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

// ����ֱ�߷�Χ
// �㷨��Դ��Graphic32
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
    if (OutCode0 = []) and (OutCode1 = []) then // ���
    begin
      Accept := True;
      AllDone := True;
    end
    else if (OutCode0 * OutCode1) <> [] then AllDone := True
    else                      // ���㽻��
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

// ����ֱ��
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
  nx := ex - px;              // ���
  ny := ey - py;              // �߶�
  if (nx = 0) or (ny = 0) then
    hyp := Round(Hypot(nx, ny))
  else
    case FPenWeight of        // б�߳�
      pwThin: hyp := Round(Hypot(nx, ny));
      pwNormal: hyp := Round(Hypot(nx, ny) * 1.4); // ��ϸ����
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
      DoSetPixelF(px, py, ARGB); // ���Ƶ�
      px := px + nx;
      py := py + ny;
    end;
    DoSetPixelF(ex, ey, ARGB); // ���ƶ˵�
  end;
  Changed;
end;

// ���Ƶ�ָ����
procedure TCnBitmap.LineToF(x, y: Single);
begin
  DrawLineF(FPenPosF.x, FPenPosF.y, x, y, FPenColor);
  MoveToF(x, y);
end;

// ���Ƶ�ָ����
procedure TCnBitmap.LineToF(Point: TPointF);
begin
  LineToF(Point.x, Point.y);
end;

// �ƶ�����
procedure TCnBitmap.MoveToF(Point: TPointF);
begin
  FPenPosF := Point;
end;

// �ƶ�����
procedure TCnBitmap.MoveToF(x, y: Single);
begin
  MoveToF(PointF(x, y));
end;

// ���ƾ���
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

// ��������
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

// ������Բ
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
  a := Abs(x2 - x1) / 2;      // �뾶
  b := Abs(y2 - y1) / 2;
  if a = 0 then
    DrawLineF(x1, y1, x1, y2, FPenColor) // ˮƽ��
  else if b = 0 then
    DrawLineF(x1, y1, x2, y1, FPenColor) // ��ֱ��
  else if a <= b then
  begin
    cx := (x1 + x2) / 2;      // ���ĵ�
    cy := (y1 + y2) / 2;

    xl1 := cx;
    xr1 := cx;
    yt1 := cy - b;
    yb1 := cy + b;
    for x := 1 to Ceil(a) do
    begin
      if x < a then           // �뾶��
      begin
        xl := cx - x;
        xr := cx + x;
        tmp := b * Sqrt(1 - Sqr(x / a));
      end else
      begin                   // �߽��
        xl := cx - a;
        xr := cx + a;
        tmp := 0;
      end;
      yt := cy - tmp;
      yb := cy + tmp;
      DrawLineF(xl1, yt1, xl, yt, FPenColor); // ���ϲ���
      DrawLineF(xl1, yb1, xl, yb, FPenColor); // ���²���
      DrawLineF(xr1, yt1, xr, yt, FPenColor); // ���ϲ���
      DrawLineF(xr1, yb1, xr, yb, FPenColor); // ���²���
      xl1 := xl;
      xr1 := xr;
      yt1 := yt;
      yb1 := yb;
    end;
  end
  else
  begin
    cx := (x1 + x2) / 2;      // ���ĵ�
    cy := (y1 + y2) / 2;

    yt1 := cy;
    yb1 := cy;
    xl1 := cx - a;
    xr1 := cx + a;
    for y := 1 to Ceil(b) do
    begin
      if y < b then           // �뾶��
      begin
        yt := cy - y;
        yb := cy + y;
        tmp := a * Sqrt(1 - Sqr(y / b));
      end else
      begin                   // �߽��
        yt := cy - b;
        yb := cy + b;
        tmp := 0;
      end;
      xl := cx - tmp;
      xr := cx + tmp;
      DrawLineF(xl1, yt1, xl, yt, FPenColor); // ���ϲ���
      DrawLineF(xl1, yb1, xl, yb, FPenColor); // ���²���
      DrawLineF(xr1, yt1, xr, yt, FPenColor); // ���ϲ���
      DrawLineF(xr1, yb1, xr, yb, FPenColor); // ���²���
      xl1 := xl;
      xr1 := xr;
      yt1 := yt;
      yb1 := yb;
    end;
  end;
  Changed;
end;

// ������Բ
procedure TCnBitmap.EllipseF(const Rect: TRectF);
begin
  EllipseF(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
end;

//--------------------------------------------------------//
// ƽ��������Ʒ���                                       //
// �㷨��ƣ��ܾ���                                       //
// �㷨��Դ��ƽ����Ч����ؼ��� AAFont V2.36���ܾ���  //
// ԭʼ�㷨�����������ṩ�� AAFont V1.2 1999.07.13        //
//           liwensong@hotmail.com                        //
//           http://member.netease.com/~lws               //
//--------------------------------------------------------//

// ȡ�ı���С
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
  // ������Ӱ
  if fsShadow in Font.StyleEx then
  begin
    Inc(Result.cx, Abs(Font.Shadow.FOffsetX));
    Inc(Result.cy, Abs(Font.Shadow.FOffsetY));
  end;
  // б���ֿ��У��
  if fsItalic in Font.Style then
    Inc(Result.cx, Round(Result.cx / Length(Text) * csItalicAdjust));
end;

// ȡ�ı��߶�
function TCnBitmap.TextHeight(const Text: string): Integer;
begin
  Result := TextExtent(Text).cy;
end;

// ȡ�ı����
function TCnBitmap.TextWidth(const Text: string): Integer;
begin
  Result := TextExtent(Text).cx;
end;

// ��ʼ���Ҷȵ���������
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

// ��ʼ������ƽ��������ƵĻҶ�ͼ
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

// �ͷ�����ƽ��������ƵĻҶ�ͼ
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

// ����ƽ�������ɰ�
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

  FGrayBmp.Canvas.Font.Assign(Font); // �趨����
  FGrayBmp.Canvas.Font.Height := FGrayBmp.Canvas.Font.Height * Font.Scale;
  FGrayBmp.Canvas.Font.Color := clWhite;
  Windows.FillRect(FGrayBmp.Canvas.Handle, Bounds(0, 0, FGrayBmp.Width,
    FGrayBmp.Height), 0);
  Windows.TextOut(FGrayBmp.Canvas.Handle, Point.x, Point.y, PChar(Text), Length(Text));

  GrayRowInc := (FGrayBmp.Width + 3) div 4 * 4; // ɨ���߿��
  pS1 := FGrayBmp.ScanLine[0]; // Դ�Ҷ�ͼ
  pS2 := PByteArray(TCnNativeInt(pS1) - GrayRowInc);
  pS3 := PByteArray(TCnNativeInt(pS2) - GrayRowInc);
  pS4 := PByteArray(TCnNativeInt(pS3) - GrayRowInc);
  pDst := FFontMask.ScanLine[0];
  // Ŀ��Ҷ�ΪԴ���ο��ƽ��ֵ
  case Font.Quality of
    fqHigh:
      begin                   // �߾��� 4x4 ����
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
      begin                   // ��ͨ���� 3x3 ����
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
      begin                   // �;��� 2x2 ����
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
      begin                   // ��ƽ��Ч��
        for I := 0 to Extend.cy - 1 do
        begin
          CopyMemory(pDst, pS1, Extend.cx);
          pS1 := PByteArray(TCnNativeInt(pS1) - GrayRowInc);
          pDst := PByteArray(TCnNativeInt(pDst) + FFontMask.FRowInc);
        end;
      end;
  end;
end;

// ����ƽ�������ɰ�
procedure TCnBitmap.DrawFontMask(const Text: string);
begin
  DrawFontMaskEx(Text, TextExtent(Text), Point(0, 0));
end;

// ������Ӱƫ��
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

// �����ı�ƫ��
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

// �ı���ǰ��ɫ�뱳�����
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

  DeRGB(AColor, r, g, b);     // ɫ�ʷ���
  for J := 0 to h - 1 do
  begin
    Src := @Mask.ScanLine[sy + J][sx];
    Dst := @ScanLine[dy + J][dx];
    for I := 0 to w - 1 do
    begin
      Weight := Src[I] * FAlpha shr 8; // ���ϵ��
      if Weight <> 0 then
      begin
        if Weight = 255 then
        begin                 // ǰ��ɫ
          Dst[I].b := b;
          Dst[I].g := g;
          Dst[I].r := r;
        end
        else
        begin                 // ���
          Inc(Dst[I].b, Weight * (b - Dst[I].b) shr 8);
          Inc(Dst[I].g, Weight * (g - Dst[I].g) shr 8);
          Inc(Dst[I].r, Weight * (r - Dst[I].r) shr 8);
        end;
      end;
    end;
  end;
end;

// �ı��������뱳�����
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
    raise EInvalidForeBmp.Create(SInvalidForeBitmap); // ���������ͼ

  FAlpha := AlphaToInt(Alpha);
  if FAlpha = 0 then Exit;

  for J := 0 to h - 1 do
  begin
    Src := @Mask.ScanLine[J + sy][sx];
    Fore := @ForeBmp.ScanLine[J + sy][sx];
    Dst := @ScanLine[J + dy][dx];
    for I := 0 to w - 1 do
    begin
      Weight := Src[I] * FAlpha shr 8; // ���ϵ��
      if Weight <> 0 then
      begin
        if Weight = 255 then
          Dst[I] := Fore[I]   // ǰ��ɫ
        else
        begin                 // ���
          Inc(Dst[I].b, Weight * (Fore[I].b - Dst[I].b) shr 8);
          Inc(Dst[I].g, Weight * (Fore[I].g - Dst[I].g) shr 8);
          Inc(Dst[I].r, Weight * (Fore[I].r - Dst[I].r) shr 8);
        end;
      end;
    end;
  end;
end;

// ƽ���ı����
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
    if fsShadow in Font.StyleEx then // ��Ӱ����
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

    DrawFontMask(Text);       // ���������ɰ�
    if fsOutline in Font.StyleEx then // ������
      FFontMask.Outline;
    if fsSpray in Font.StyleEx then // �罦Ч��
      FFontMask.Spray(Font.Spray);

    if not FontClear then     // ������͸��
      FillRect(Bounds(x, y, FFontMask.Width, FFontMask.Height), FontBkColor);

    if fsShadow in Font.StyleEx then // ��Ӱ����
    begin
      ShadowMask := TCnFontMask.Create; // ��Ӱ�ɰ�
      try
        ABlur := Font.Shadow.Blur;
        if ABlur > 0 then     // ����ģ��
        begin
          ShadowMask.SetSize(FFontMask.Width + 4 * ABlur, FFontMask.Height +
            4 * ABlur);
          with ShadowMask do
            FillChar(FBuff^, FRowInc * FHeight, 0);
          for I := 0 to FFontMask.FHeight - 1 do
            Move(FFontMask.ScanLine[I][0], ShadowMask.ScanLine[2 * ABlur + I][2 *
              ABlur],
                FFontMask.Width);
          ShadowMask.Blur(ABlur); // ��Ӱģ��
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
      not Font.Texture.Graphic.Empty; // ��������
    IsGrad := fsGradient in Font.StyleEx; // ���彥��
    IsLight := fsLighting in Font.StyleEx; // ����Ч��
    IsNoise := (fsNoise in Font.StyleEx) and (Font.Noise > 0); // ����Ч��
    if IsTexture or IsGrad or IsLight or IsNoise then
    begin
      Fore := TCnBitmap.Create;
      try
        Fore.SetSize(FFontMask.Width, FFontMask.Height);
        if not IsGrad then
          Fore.Fill(Font.Color)  // �޽���Ч��ʱ�������ɫ
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

// �Ӹ��ؼ����Ʊ���������������� RxLibrary VCLUtils
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

initialization
  BitmapList := TThreadList.Create;
  BitmapList.duplicates := dupIgnore;   // �ظ�����ʱ����
  CnCanvasList := TThreadList.Create;
  CnCanvasList.duplicates := dupIgnore; // �ظ�����ʱ����
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

