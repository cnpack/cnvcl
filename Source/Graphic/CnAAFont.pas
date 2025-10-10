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

unit CnAAFont;
{* |<PRE>
================================================================================
* ������ƣ�CnPack �ؼ���
* ��Ԫ���ƣ�ƽ����Ч���嵥Ԫ
* ��Ԫ���ߣ�CnPack ������ �ܾ��� (zjy@cnpack.org)
*           ��ֲ��e- 
*           ����
* ����ƽ̨��PWin2000Pro + Delphi 5.01
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7/2005 + C++Build 5/6
* ��    ע��ƽ�������㷨�������������ṩ�� AAFont �޸Ķ���
* �����£�2021.07.23
*               TCnAABlend �� BlendEx �����Լ� TCnAAFontEx �� TextOutput ����֧��
*               Ŀ��Ϊ 32 λ�� Alpha ͨ����λͼ����Ҫ������ָ�� DestIsAlpha Ϊ True
*               ע��ԴͼҪ UnPreMultiply �ģ����Ϊ PreMultiply �ġ�
*           2021.07.17
*               TCnAABlend �� Blend �����Լ� TCnAAFont �� TextOutput ����֧��Ŀ
*               ��Ϊ 32 λ�� Alpha ͨ����λͼ����Ҫ������ָ�� DestIsAlpha Ϊ True
*               ע���� XE �����°汾�� 32 λ Bitmap �� Alpha ͨ��֧�ֲ�������
*               Ҫ��ʾ TCnAAFont ������Ĵ�͸���ȵ�ƽ������ͼ����Ҫ���е���
*               Windows.AlphaBlend ����ʾ��
* ��ֲ���ڣ�2015.06.15
*               �޸�������Զ�� BCB Unicode ����������������
* ��ֲ���ڣ�2006.08.18
*           2004.11.29
*               д��
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, {$IFDEF FPC} JwaWindows, {$ELSE} Consts, {$ENDIF} Messages, Classes, Graphics, SysUtils,
  Controls, Forms, Registry, StdCtrls, ExtCtrls, Math, IniFiles, CnNative, CnClasses;

type

  TAAQuality = (aqHigh, aqNormal, aqLow, aqNone);
  {* ƽ��������ʾ��������
   |<PRE>
     aqHigh     - 4X4 �����������ʾ���ȣ��ٶȽ���
     aqNormal   - 3X3 ��������ͨ��ʾ���ȣ���������ٶȱ�
     aqLow      - 2X2 �����ĵ���ʾ���ȣ��ٶȽϿ�
     aqNone     - ��ƽ��Ч��
   |</PRE>}
  TAlpha = 0..100;
  {* ��͸�������ͣ�0 Ϊ��ȫ͸����100 Ϊ��ȫ��͸��}
  TBlurStrength = 0..100;
  {* ģ�������ͣ�0 Ϊ��ģ����100 Ϊ���ģ����}
  TOffset = -20..20;
  {* ��Ӱƫ������Χ}
  TSprayRange = 0..100;
  {* �罦Ч��������Χ}
  TAngle = -360..360;
  {* ��ת�Ƕ�}
  TGradualStyle = (gsLeftToRight, gsRightToLeft, gsTopToBottom, gsBottomToTop,
    gsCenterToLR, gsCenterToTB);
  {* ���䷽ʽ����
   |<PRE>
     gsLeftToRight      - �������ҽ���
     gsRightToLeft      - �������󽥱�
     gsTopToBottom      - �������½���
     gsBottomToTop      - �������Ͻ���
     gsCenterToLR       - ���м������ҽ���
     gsCenterToTB       - ���м������½���
   |</PRE>}
  TTextureMode = (tmTiled, tmStretched, tmCenter, tmNormal);
  {* ����ͼ����ʾģʽ
   |<PRE>
     tmTiled            - ƽ����ʾ
     tmStretched        - �Զ�������ʾ
     tmCenter           - ������λ����ʾ
     tmNormal           - �����Ͻ���ʾ
   |</PRE>}
  THoriScrollType = (stNone, stRightToLeft, stLeftToRight);
  {* �����������
   |<PRE>
     stNone             - ����
     stRightToLeft      - ���ҵ������
     stLeftToRight      - �����ҹ���
   |</PRE>}

  TCnAAEffect = class;
  TCnAAMask = class;
  TCnAABlend = class;
  TCnAAFont = class;
  TCnAAFontEx = class;

{ TCnEnabledClass }

  TCnEnabledClass = class(TCnNotifyClass)
  {* �� Enabled ���ܵĸ���֪ͨ�ĳ־����࣬һ�㲻��Ҫֱ��ʹ��}
  private
    FEnabled: Boolean;
  protected
    procedure SetEnabled(const Value: Boolean); virtual;
    procedure Changed; override;
  public
    constructor Create(ChangedProc: TNotifyEvent); override;
    {* �๹����������Ϊ֪ͨ�¼�}
    procedure Assign(Source: TPersistent); override;
    {* ����ֵ����}
  published
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    {* �Ƿ�����ʹ��}
  end;

{ TCnAAShadow }

  TCnAAShadow = class(TCnEnabledClass)
  {* ƽ����Ч������Ӱ��ʾ�����࣬һ�㲻��Ҫ�û�ֱ�Ӵ���}
  private
    FBlur: TBlurStrength;
    FAlpha: TAlpha;
    FColor: TColor;
    FOffsetX: TOffset;
    FOffsetY: TOffset;
    procedure SetBlur(const Value: TBlurStrength);
    procedure SetColor(const Value: TColor);
    procedure SetOffsetX(const Value: TOffset);
    procedure SetOffsetY(const Value: TOffset);
    procedure SetAlpha(const Value: TAlpha);
  public
    constructor Create(ChangedProc: TNotifyEvent); override;
    {* �๹����������Ϊ֪ͨ�¼�}
    procedure Assign(Source: TPersistent); override;
    {* ����ֵ����}
  published
    property Blur: TBlurStrength read FBlur write SetBlur default 80;
    {* ��Ӱģ���ȣ�������ΧΪ 0..100��0 Ϊ��ģ��}
    property Alpha: TAlpha read FAlpha write SetAlpha default 70;
    {* ��Ӱ��͸���ȣ�������ΧΪ 0..100��0 Ϊȫ͸����100 Ϊ��͸��}
    property Color: TColor read FColor write SetColor default $00444444;
    {* ��Ӱ��ɫ}
    property OffsetX: TOffset read FOffsetX write SetOffsetX default 2;
    {* ��Ӱˮƽ����ƫ������Ϊ��ֵʱ����ƫ��}
    property OffsetY: TOffset read FOffsetY write SetOffsetY default 2;
    {* ��Ӱ��ֱ����ƫ������Ϊ��ֵʱ����ƫ��}
  end;

{ TCnAAGradual }

  TCnAAGradual = class(TCnEnabledClass)
  {* ƽ����Ч���彥����ʾ�����࣬һ�㲻��Ҫ�û�ֱ�Ӵ���}
  private
    FStyle: TGradualStyle;
    FStartColor: TColor;
    FEndColor: TColor;
    procedure SetStyle(const Value: TGradualStyle);
    procedure SetStartColor(const Value: TColor);
    procedure SetEndColor(const Value: TColor);
  public
    constructor Create(ChangedProc: TNotifyEvent); override;
    {* �๹����������Ϊ֪ͨ�¼�}
    procedure Assign(Source: TPersistent); override;
    {* ����ֵ����}
  published
    property Style: TGradualStyle read FStyle write SetStyle default gsLeftToRight;
    {* ������ʾ���}
    property StartColor: TColor read FStartColor write SetStartColor
      default clWhite;
    {* ������ʼɫ}
    property EndColor: TColor read FEndColor write SetEndColor default clBlack;
    {* �������ɫ}
  end;

{ TCnAATexture }

  TCnAATexture = class(TCnEnabledClass)
  {* ƽ����Ч������������࣬һ�㲻��Ҫ�û�ֱ�Ӵ���}
  private
    FPicture: TPicture;
    FMode: TTextureMode;
    procedure SetMode(const Value: TTextureMode);
    procedure SetPicture(const Value: TPicture);
    procedure PictureChanged(Sender: TObject);
  public
    constructor Create(ChangedProc: TNotifyEvent); override;
    {* �๹����������Ϊ֪ͨ�¼�}
    destructor Destroy; override;
    {* ��������}
    procedure Assign(Source: TPersistent); override;
    {* ����ֵ����}
  published
    property Mode: TTextureMode read FMode write SetMode default tmTiled;
    {* ����ͼ����ʾģʽ}
    property Picture: TPicture read FPicture write SetPicture;
    {* ����ͼ��}
  end;

{ TCnAAEffect }

  TCnAAEffect = class(TCnNotifyClass)
  {* ƽ����Ч��������࣬һ�㲻��Ҫ�û�ֱ�Ӵ���}
  private
    FAlpha: TAlpha;
    FBlur: TBlurStrength;
    FGradual: TCnAAGradual;
    FShadow: TCnAAShadow;
    FTexture: TCnAATexture;
    FOutline: Boolean;
    FNoise: Byte;
    FSpray: TSprayRange;
    FAngle: TAngle;
    FHorzMirror: Boolean;
    FVertMirror: Boolean;
    procedure SetAlpha(const Value: TAlpha);
    procedure SetBlur(const Value: TBlurStrength);
    procedure SetGradual(const Value: TCnAAGradual);
    procedure SetShadow(const Value: TCnAAShadow);
    procedure SetTexture(const Value: TCnAATexture);
    procedure SetOutline(const Value: Boolean);
    procedure SetNoise(const Value: Byte);
    procedure SetSpray(const Value: TSprayRange);
    procedure SetAngle(const Value: TAngle);
    procedure SetHorzMirror(const Value: Boolean);
    procedure SetVertMirror(const Value: Boolean);
  public
    constructor Create(ChangedProc: TNotifyEvent); override;
    {* �๹����������Ϊ֪ͨ�¼�}
    destructor Destroy; override;
    {* ��������}
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromIni(Ini: TCustomIniFile; const Section: string); virtual;
    procedure SaveToIni(Ini: TCustomIniFile; const Section: string); virtual;
    {* ����ֵ����}
  published
    property Shadow: TCnAAShadow read FShadow write SetShadow;
    {* ������Ӱ����}
    property Gradual: TCnAAGradual read FGradual write SetGradual;
    {* ������ʾ����}
    property Texture: TCnAATexture read FTexture write SetTexture;
    {* �����������}
    property Alpha: TAlpha read FAlpha write SetAlpha default 100;
    {* ��͸���ȣ�������ΧΪ 0..100��0 Ϊ��ȫ͸����100 Ϊ��͸��}
    property Blur: TBlurStrength read FBlur write SetBlur default 0;
    {* ģ���ȣ�������ΧΪ 0..100��0 Ϊ��ģ��}
    property Angle: TAngle read FAngle write SetAngle default 0;
    {* ������ת�Ƕȣ�������ΧΪ -360..360����λΪ��}
    property Noise: Byte read FNoise write SetNoise default 0;
    {* ����Ч����������ΧΪ 0..255}
    property Spray: TSprayRange read FSpray write SetSpray default 0;
    {* �罦Ч����������ΧΪ 0..100}
    property Outline: Boolean read FOutline write SetOutline default False;
    {* �Ƿ���ʾ����Ч��}
    property HorzMirror: Boolean read FHorzMirror write SetHorzMirror default False;
    {* �Ƿ�ˮƽ����}
    property VertMirror: Boolean read FVertMirror write SetVertMirror default False;
    {* �Ƿ�ֱ����}
  end;

{ TCnAAMask }

  EInvalidPixel = class(Exception);
  EInvalidLine = class(Exception);

  PByteArray = ^TByteArray;
  {* �ֽ�����ָ��}
  TByteArray = array[0..32767] of Byte;
  {* �ֽ���������}

  PRGBArray = ^TRGBArray;
  {* RGB ����ָ��}
  TRGBArray = array[0..8192] of tagRGBTriple;
  {* RGB ��������}

  PBGRAArray = ^TBGRAArray;
  {* BGRA ����ָ��}
  TBGRAArray = array[0..8192] of tagRGBQUAD;
  {* BGRA ��������}

  TCnAAMask = class(TPersistent)
  {* ƽ����Ч�����ɰ崦���࣬������ƽ������ʱ�ڲ�ʹ��}
  private
    FQuality: TAAQuality;
    FPMaskBuff: PByteArray; // �洢�����ľ���ƽ������� 8bit �Ҷ�����
    FHeight: Integer;
    FWidth: Integer;
    BytesLineGray: Integer;
    BytesLineMask: Integer;
    Scale: Integer;
    FAAFont: TCnAAFont;
    FGrayBmp: TBitmap;
    procedure InitGrayBmp;
    procedure FreeGrayBmp;
    procedure SetQuality(const Value: TAAQuality);
    function TextExtentEx(const S: string; var Point: TPoint): TSize;
  protected
    function ScanLine(Line: Integer; pAData: PByteArray): PByteArray; overload;
    property pMaskBuff: PByteArray read FPMaskBuff;
  public
    constructor Create(AOwner: TCnAAFont);
    {* �๹����}
    destructor Destroy; override;
    {* ��������}
    procedure Assign(Source: TPersistent); override;
    {* ����ֵ����}
    procedure DrawMask(const Text: string);
    {* �����ı��ɰ�ͼ}
    procedure DrawMaskEx(const Text: string; Extend: TSize; Point: TPoint);
    {* �����ı��ɰ�ͼ��ǿ��}
    procedure Blur(Blur: TBlurStrength);
    {* ���ɰ�ͼ����ģ������}
    procedure Outline;
    {* ���ɰ�ͼ������������}
    procedure Spray(Amount: Integer);
    {* ���ɰ�ͼ�����罦����}
    procedure HorzMirror;
    {* ���ɰ�ͼ����ˮƽ������}
    procedure VertMirror;
    {* ���ɰ�ͼ���д�ֱ������}
    function TextExtent(const S: string): TSize;
    {* �����ı��ߡ���}
    function TextHeight(const S: string): Integer;
    {* �����ı��߶�}
    function TextWidth(const S: string): Integer;
    {* �����ı����}
    function ScanLine(Line: Integer): Pointer; overload;
    {* �����ɰ�ͼɨ���ߵ�ַ}
    function Pixel(X, Y: Integer): Byte;
    {* �����ɰ�ͼָ�����ػҶ�ֵ}
    function PixelAddr(X, Y: Integer): Pointer;
    {* �����ɰ�ͼָ�����ص�ַ}
    property Height: Integer read FHeight;
    {* �ɰ�ͼ�ĸ߶�}
    property Width: Integer read FWidth;
    {* �ɰ�ͼ�Ŀ��}
    property Quality: TAAQuality read FQuality write SetQuality;
    {* ƽ��������ƾ���}
  end;

{ TCnAABlend }

  EInvalidForeBmp = class(Exception);

  TCnAABlend = class(TPersistent)
  {* ƽ����Ч����ͼ���ϴ����࣬������ƽ������ʱ�ڲ�ʹ��}
  private
    FForeBmp: TBitmap;
    FRGBBmp: TBitmap;
    FForeBmp32: TBitmap;
    FBGRABmp: TBitmap;
    FAAFont: TCnAAFont;
    procedure SetForeBmp(const Value: TBitmap);
    procedure SetForeBmp32(const Value: TBitmap);
  public
    constructor Create(AOwner: TCnAAFont);
    {* �๹����}
    destructor Destroy; override;
    {* ��������}
    procedure Assign(Source: TPersistent); override;
    {* ����ֵ����}
    procedure SyncForeBmp32;
    {* �� 24 λǰ��ͼ�������� 32 λǰ��ͼ}
    procedure Blend(X, Y: Integer; AColor: TColor; Alpha: TAlpha;
      Mask: TCnAAMask; DestIsAlpha: Boolean = False);
    {* ��ָ����ɫ���л�ϣ���Ŀ����Ҫ֧�� Alpha ͸����ʱ��ָ�� DestIsAlpha Ϊ True
      ע�� DestIsAlpha Ϊ True ʱ��Ҫ�� AAFont.Canvas �е�Դ�� 32 λ
        �����ڲ�֧�� AlphaFormat ������£�Ҫ�������� UnPreMultiply �ģ��ڲ����� PreMultiply��
      ������� PreMultiply ��������}
    procedure BlendEx(X, Y: Integer; Alpha: TAlpha; Mask: TCnAAMask;
      DestIsAlpha: Boolean = False);
    {* ʹ��ǰ��ͼ���л�ϣ�DestIsAlpha Ϊ True ʱʹ�� ForeBmp32������ʹ�� ForeBmp
      ע�� DestIsAlpha Ϊ True ʱ��Ҫ�� ForeBmp32 �ڲ�֧�� AlphaFormat �������
        Ҫ�������� PreMultiply �ģ��ڲ����� UnPreMultiply��
      ������� PreMultiply �������� }
    property ForeBmp: TBitmap read FForeBmp write SetForeBmp;
    {* ����ǰ��ͼ��24 λ RGB ɫ�ʵ�}
    property ForeBmp32: TBitmap read FForeBmp32 write SetForeBmp32;
    {* ����ǰ��ͼ��32 λ Alpha ͨ���ģ������� UnPreMultiply ��}
  end;

{ TCnAAFont }

  TCnAAFont = class
  {* ƽ����Ч��������࣬��װ�˻�����ƽ��������Ʒ������û����ֶ�ʹ�á�
   |<BR>�����Ҫ�������ط�����ƽ�����壬��ʹ������ķ�����
   !var
   !  AAFont: TCnAAFont;
   !  W, H: Integer;
   !  S: string;
   !begin
   !  // ���� TCnAAFont ʵ������ָ����ʹ�� PaintBox1 �Ļ������л���
   !  AAFont := TCnAAFont.Create(PaintBox1.Canvas);
   !  try
   !    with PaintBox1.Canvas do
   !    begin
   !      Font.Name := '����'; // ��������
   !      Font.Size := 24;
   !      Font.Color := clBlue;
   !      Brush.Style := bsClear; // ����͸������
   !    end;
   !    S := '����һ��ʹ��ƽ�����������';
   !    W := AAFont.TextWidth(S);
   !    H := AAFont.TextHeight(S);
   !    with PaintBox1 do // �ڿؼ���������ı�
   !      AAFont.TextOutput((Width - W) div 2, (Height - H) div 2, S, 80, 0);
   !    AAFont.Canvas := Image1.Canvas; // Ҳ�����л�����һ����
   !    AAFont.TextOut(10, 10, S); // ����ʱ��ʹ��Image1.Canvas����������
   !  finally
   !    AAFont.Free;
   !  end;
   !end;}
  private
    FCanvas: TCanvas;
    function GetQuality: TAAQuality;
    procedure SetQuality(const Value: TAAQuality);
  protected
    Mask: TCnAAMask;
    Blend: TCnAABlend;
  public
    constructor Create(ACanvas: TCanvas); virtual;
    {* �๹����������Ϊ����ƽ�������ı��ͼ����ı���Сʱʹ�õĻ�����
     |<BR> ����Ϊ nil�����Ϊ nil�����ڵ����ı�����ǰ�� Canvas ���Ը�ֵ}
    destructor Destroy; override;
    {* ��������}
    procedure TextOutput(X, Y: Integer; const S: string; Alpha: TAlpha = 100;
      Blur: TBlurStrength = 0; DestIsAlpha: Boolean = False);
    {* ���ƽ�������ı�����ǰ���õ� Canvas �У�ʹ�������������Ժͻ�ˢ���á�
     |<BR> ���Ҫ�������͸�����ı�����Ҫ�� Canvas.Brush.Style ��Ϊ bsClear��
     |<BR> ע���÷�����֧�ֶ����ı���
     |<PRE>
       X, Y: Integer    - �ı����λ��
       S: string        - Ҫ���Ƶ��ַ���
       Alpha: TAlpha    - �ı��Ĳ�͸���ȣ�0~100��Ĭ��Ϊ��ȫ��͸�� 100
       Blur: TBlurStrength  - �ı���ģ���ȣ�Ĭ��Ϊ������ģ������
       DestIsAlpha: Boolean - ��Ŀ����Ҫ֧�� Alpha ͸����ʱ��ָ�� DestIsAlpha Ϊ True
     |</PRE>}
    function TextExtent(const S: string): TSize; virtual;
    {* �����ı��ߡ���}
    function TextHeight(const S: string): Integer; virtual;
    {* ����ָ���ı�����ʾ�߶ȣ�ʹ�õ�ǰ�� Canvas ����}
    function TextWidth(const S: string): Integer; virtual;
    {* ����ָ���ı�����ʾ��ȣ�ʹ�õ�ǰ��Canvas����}
    property Quality: TAAQuality read GetQuality write SetQuality;
    {* ƽ��������ƾ���}
    property Canvas: TCanvas read FCanvas write FCanvas;
    {* ����ƽ���������������ı��ߴ���Ļ���}
  end;

{ TCnAAFontEx }

  TCnAAFontEx = class(TCnAAFont)
  {* ��չ��ƽ����Ч��������࣬ʵ������Ӱ�����䡢�������Ч��
   |<BR> �û����ֶ����� TCnAAFontEx ���������ƴ���Ч��ƽ�������ı���
   ʹ�÷��������� TCnAAFont��}
  private
    FEffect: TCnAAEffect;
    procedure SetEffect(const Value: TCnAAEffect);
  protected
    function GetShadowPoint: TPoint;
    function GetTextPoint: TPoint;
    procedure CreateGradual;
    procedure DrawTiled(Canvas: TCanvas; Rect: TRect; G: TGraphic);
    procedure CreateForeBmp;
    procedure CreateNoiseBmp;
    procedure AddNoise(Amount: Byte);
  public
    constructor Create(ACanvas: TCanvas); override;
    {* �๹����������Ϊ����ƽ�������ı��ͼ����ı���Сʱʹ�õĻ�����
     |<BR> ����Ϊ nil�����Ϊ nil�����ڵ����ı�����ǰ�� Canvas ���Ը�ֵ}
    destructor Destroy; override;
    {* ��������}
    function TextExtent(const S: string): TSize; override;
    {* �����ı��ߡ���
     |<BR> ע��Effect �����е���Ӱ����ת�Ƕȵ����ý�Ӱ�췵�ؽ��}
    procedure TextOutput(X, Y: Integer; const S: string; DestIsAlpha: Boolean = False);
    {* ʹ�� Effect ���õ�������Ч�����ƽ�������ı�����ǰ���õ� Canvas �У�ʹ�������������Ժͻ�ˢ���á�
     |<BR> ���Ҫ�������͸�����ı�����Ҫ�� Canvas.Brush.Style ��Ϊ bsClear��
     |<BR> ע���÷�����֧�ֶ����ı���
     |<PRE>
       X, Y: Integer    - �ı����λ��
       S: string        - Ҫ���Ƶ��ַ���
       DestIsAlpha: Boolean - ��Ŀ����Ҫ֧�� Alpha ͸����ʱ��ָ�� DestIsAlpha Ϊ True
     |</PRE>}
    property Effect: TCnAAEffect read FEffect write SetEffect;
    {* ƽ���������ʱ����Ч����}
  end;

const
  csMaxProgress = 255;

type
  TCnFontLabel = class;
  TCnFontLabels = class;
  TCnUserLabel = class;
  TCnUserLabels = class;
  TCnAAGraphicControl = class;

{ TCnFontLabel }

  TCnFontLabel = class(TCollectionItem)
  {* �����ǩ�б����࣬TCnFontLabels �����һ�㲻��Ҫ�û�ֱ�Ӵ���}
  private
    FName: string;
    FFont: TFont;
    FEffect: TCnAAEffect;
    function GetFontLabels: TCnFontLabels;
    procedure Changed;
    procedure SetFont(const Value: TFont);
    procedure SetName(const Value: string);
    procedure OnEffectChanged(Sender: TObject);
    procedure SetEffect(const Value: TCnAAEffect);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    {* �๹����}
    destructor Destroy; override;
    {* ��������}
    procedure Assign(Source: TPersistent); override;
    {* ����ֵ����}
    property FontLabels: TCnFontLabels read GetFontLabels;
    {* ������}
  published
    property Name: string read FName write SetName;
    {* �����ǩ��}
    property Font: TFont read FFont write SetFont;
    {* ��������}
    property Effect: TCnAAEffect read FEffect write SetEffect;
    {* ƽ��������Ч��ʾ����}
  end;

{ TCnFontLabels }

  TCnFontLabels = class(TOwnedCollection)
  {* �����ǩ�б��࣬���������ı���ؼ��п�ʹ�õ������ǩ����һ�㲻��Ҫ�û�ֱ�Ӵ���}
  private
    FOnChanged: TNotifyEvent;
    function GetItem(Index: Integer): TCnFontLabel;
    procedure SetItem(Index: Integer; const Value: TCnFontLabel);
  protected
    procedure Changed;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TComponent);
    {* �๹����}
    function AddItem(AName: string; AFontName: string; AFontSize: Integer;
      AFontColor: TColor; AFontEffect: TFontStyles; Shadow: Boolean;
      OffsetX, OffsetY: Integer): TCnFontLabel;
    {* ����һ���µ������ǩ}
    function IndexOf(const Name: string): Integer;
    {* ���ݱ�ǩ����������������}
    procedure Check(var AText: string; AFont: TFont; AEffect: TCnAAEffect);
    {* �����ܴ������ǩ���ַ�����
     |<BR> ����ҵ���Ӧ�ı�ǩ��ɾ���ַ����еı�ǩ������<>��ǣ������øñ�ǩ
       ����� Font �� Effect �������ò����еĶ�Ӧ����}
    property Items[Index: Integer]: TCnFontLabel read GetItem write SetItem; default;
    {* �����ǩ����������}
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    {* ���Ա��֪ͨ}
  end;

{ TCnUserLabel }

  TGetTextEvent = procedure(Sender: TCnUserLabel; var Text: string) of object;
  {* ȡ�û���ǩ����Ӧ���ı��¼�
   |<PRE>
     Sender: TCnUserLabel  - �������¼��Ķ���
     Text: string        - ���û����ظñ�ǩ��Ӧ���ı�����������
   |</PRE>}
  TLabelStyle = (lsLeftJustify, lsCenter, lsRightJustify, lsRegOwner,
    lsRegOrganization, lsAppTitle, lsDate, lsTime, lsCustom);
  {* �û���ǩ���ͣ��ֶ�����Ʊ�ǩ���ı���ǩ���ı���ǩ������ʱ��ָ�����ı�ȡ����
   |<PRE>
     lsLeftJustify      - ������ǩ�����ɼ����Ʊ�ǩ�������ı����뷽ʽ
     lsCenter           - ���Ķ����ǩ�����ɼ����Ʊ�ǩ�������ı����뷽ʽ
     lsRightJustify     - �Ҷ����ǩ�����ɼ����Ʊ�ǩ�������ı����뷽ʽ
     lsRegOwner         - ע������û�����ǩ��ϵͳ�����ǩ
     lsRegOrganization  - ע�������֯����ǩ��ϵͳ�����ǩ��NT����Ч��
     lsAppTitle         - Ӧ�ó�������ǩ��ϵͳ�����ǩ
     lsDate             - ��ǰ���ڱ�ǩ��ϵͳ�����ǩ
     lsTime             - ��ǰʱ���ǩ��ϵͳ�����ǩ
     lsCustom           - �û��Զ����ǩ����
   |</PRE>}

  TCnUserLabel = class(TCollectionItem)
  {* �û��ı���ǩ�б����࣬TCnUserLabels �����һ�㲻��Ҫ�û�ֱ�Ӵ���}
  private
    FName: string;
    FText: string;
    FOnGetText: TGetTextEvent;
    FStyle: TLabelStyle;
    function GetUserLabels: TCnUserLabels;
    procedure Changed;
    procedure SetName(const Value: string);
    procedure SetText(const Value: string);
    function GetText: string;
    procedure SetStyle(const Value: TLabelStyle);
    function IsTextStored: Boolean;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    {* �๹����}
    procedure Assign(Source: TPersistent); override;
    {* ����ֵ����}
    property UserLabels: TCnUserLabels read GetUserLabels;
    {* ������}
  published
    property Name: string read FName write SetName;
    {* �û���ǩ��}
    property Text: string read GetText write SetText stored IsTextStored;
    {* ��ǩ����Ӧ���ı�����������ʾ�ı��еı�ǩ�ø�ֵ����}
    property Style: TLabelStyle read FStyle write SetStyle default lsCustom;
    {* ��ǩ����}
    property OnGetText: TGetTextEvent read FOnGetText write FOnGetText;
    {* ȡ�û���ǩ����Ӧ���ı��¼�����ϵͳ��ǩҲ��Ч}
  end;

{ TCnUserLabels }

  TCnUserLabels = class(TOwnedCollection)
  {* �û���ǩ�б��࣬���������ı���ؼ��п�ʹ�õ��û���ǩ����һ�㲻��Ҫ�û�ֱ�Ӵ���}
  private
    RegOwner: string;
    RegOrganization: string;
    FOnChanged: TNotifyEvent;
    function GetItem(Index: Integer): TCnUserLabel;
    procedure SetItem(Index: Integer; const Value: TCnUserLabel);
    procedure InitRegInfo;
  protected
    procedure Changed;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TComponent);
    {* �๹����}
    function AddItem(AName: string; AText: string; AStyle: TLabelStyle): TCnUserLabel;
    {* ����һ���µ��û���ǩ}
    function IndexOf(const Name: string): Integer;
    {* ���ݱ�ǩ��������������}
    procedure Check(var AText: string; var Align: TAlignment);
    {* �����ܴ��ı���ǩ���ַ�����
     |<BR> ����ҵ���Ӧ�ı�ǩ�����ı��еı�ǩ������<>���ţ��ñ�ǩ�� Text ����ȡ����
       ͬʱ���������ñ�ǩ�� OnGetText �¼�������Ƕ����ǩ�������ò����е� Align ���ԡ�}
    property Items[Index: Integer]: TCnUserLabel read GetItem write SetItem; default;
    {* �û���ǩ����������}
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    {* ���Ա��֪ͨ}
  end;

{ TCnPackParam }

  TCnPackParam = class(TPersistent)
  {* ����Ĳ���������}
  private
    FOwner: TControl;
  protected
    property Owner: TControl read FOwner;
  public
    constructor Create(AOwner: TControl); virtual;
    {* �๹����}
    procedure Assign(Source: TPersistent); override;
    {* ����ֵ����}
  end;

{ TCnDrag }

  TCnDrag = class(TCnPackParam)
  {* ������Ϸ����������}
  private
    function GetDragCursor: TCursor;
    function GetDragKind: TDragKind;
    function GetDragMode: TDragMode;
    procedure SetDragCursor(const Value: TCursor);
    procedure SetDragKind(const Value: TDragKind);
    procedure SetDragMode(const Value: TDragMode);
  published
    property DragKind: TDragKind read GetDragKind write SetDragKind default dkDrag;
    {* �Ϸ����ͣ�ͬ TControl �ж���}
    property DragCursor: TCursor read GetDragCursor write SetDragCursor default crDrag;
    {* �ϷŹ�꣬ͬ TControl �ж���}
    property DragMode: TDragMode read GetDragMode write SetDragMode default dmManual;
    {* �Ϸ�ģʽ��ͬ TControl �ж���}
  end;

{ TCnParentEffect }

  TCnParentEffect = class(TCnPackParam)
  {* ����ĸ��ؼ�Ӱ�����������}
  private
    function GetParentBiDiMode: Boolean;
    function GetParentColor: Boolean;
    function GetParentFont: Boolean;
    function GetParentShowHint: Boolean;
    procedure SetParentBiDiMode(const Value: Boolean);
    procedure SetParentColor(const Value: Boolean);
    procedure SetParentFont(const Value: Boolean);
    procedure SetParentShowHint(const Value: Boolean);
  protected
    property ParentBiDiMode: Boolean read GetParentBiDiMode write SetParentBiDiMode
      default True;
  published
    property ParentColor: Boolean read GetParentColor write SetParentColor default
      True;
    {* ʹ�ø��ؼ�����ɫ��ͬ TControl �ж���}
    property ParentFont: Boolean read GetParentFont write SetParentFont default True;
    {* ʹ�ø��ؼ������壬ͬ TControl �ж���}
    property ParentShowHint: Boolean read GetParentShowHint write SetParentShowHint
      default True;
    {* ʹ�ø��ؼ�����ʾ��ʾ���ã�ͬ TControl �ж���}
  end;

{ TCnCustomParam }

  TBackGroundMode = (bmTiled, bmStretched, bmCenter, bmNormal);
  {* ����ͼ����ʾģʽ
   |<PRE>
     bmTiled            - ƽ����ʾ
     bmStretched        - �Զ�������ʾ
     bmCenter           - ������λ����ʾ
     bmNormal           - �����Ͻ���ʾ
   |</PRE>}

  TCnCustomParam = class(TCnNotifyClass)
  {* �ɶ��Ƶ�ƽ������ؼ��������࣬һ�㲻��Ҫ�û�ֱ�Ӵ���}
  private
    FAlignment: TAlignment;
    FWordWrap: Boolean;
    FTransparent: Boolean;
    FLayout: TTextLayout;
    FOwner: TCnAAGraphicControl;
    FBackGround: TPicture;
    FBackGroundMode: TBackGroundMode;

    procedure BackGroundChanged(Sender: TObject);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetLayout(const Value: TTextLayout);
    procedure SetTransparent(const Value: Boolean);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetQuality(const Value: TAAQuality);
    procedure SetFontEffect(const Value: TCnAAEffect);
    function GetQuality: TAAQuality;
    function GetFontEffect: TCnAAEffect;
    function GetColor: TColor;
    function GetFont: TFont;
    procedure SetColor(const Value: TColor);
    procedure SetFont(const Value: TFont);
    procedure SetBackGround(const Value: TPicture);
    procedure SetBackGroundMode(const Value: TBackGroundMode);
    function IsColorStroed: Boolean;
  protected
    function IsBackEmpty: Boolean;
    property Owner: TCnAAGraphicControl read FOwner;
    property Font: TFont read GetFont write SetFont;
    {* �ؼ�����}
    property Quality: TAAQuality read GetQuality write SetQuality default aqNormal;
    {* ƽ��������ʾ����}
    property Alignment: TAlignment read FAlignment write SetAlignment
      default taLeftJustify;
    {* �ı����뷽ʽ}
    property Layout: TTextLayout read FLayout write SetLayout default tlTop;
    {* �ı���ֱ������뷽ʽ}
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
    {* �ı��Ƿ��Զ�����}
    property Transparent: Boolean read FTransparent write SetTransparent
      default False;
    {* �ؼ��Ƿ�͸��}
    property FontEffect: TCnAAEffect read GetFontEffect write SetFontEffect;
    {* ƽ����Ч��������}
    property BackGround: TPicture read FBackGround write SetBackGround;
    {* �ؼ�����ͼ��}
    property BackGroundMode: TBackGroundMode read FBackGroundMode
      write SetBackGroundMode default bmCenter;
    {* �ؼ�����ͼ����ʾģʽ}
    property BackColor: TColor read GetColor write SetColor stored IsColorStroed;
    {* �ؼ�������ɫ}
  public
    constructor Create(AOwner: TCnAAGraphicControl; ChangedProc: TNotifyEvent);
      reintroduce; virtual;
    {* �๹����}
    destructor Destroy; override;
    {* ��������}
    procedure Assign(Source: TPersistent); override;
    {* ����ֵ����}
  end;

{ TCnCustomTextParam }

  TLabelEffect = (leOnlyALine, leUntilNextLabel);
  {* ��ǩ���÷�Χ����
   |<PRE>
     leOnlyALine        - ���塢���Ʊ�ǩ���ڵ�ǰ����Ч���ޱ�ǩ�������塢�������Ĭ��ֵ����
     leUntilNextLabel   - ��ǩ����Ӧ�Ĳ���Ӱ�쵱ǰ�кͺ����У�ֱ��������һ��ǩΪֹ
   |</PRE>}
  TRowPitch = -100..150;
  {* �м�����ͣ���λΪ����߶ȵİٷֱȣ�����Ϊ���Բ�������Ч��}

  TCnCustomTextParam = class(TCnCustomParam)
  {* �ɶ��Ƶ�ƽ�������ı���ؼ��������࣬һ�㲻��Ҫ�û�ֱ�Ӵ���}
  private
    FLines: TStrings;
    FLabelEffect: TLabelEffect;
    FRowPitch: TRowPitch;
    FFontEffect: TCnAAEffect;

    procedure LinesChanged(Sender: TObject);
    procedure SetLines(const Value: TStrings);
    procedure SetLabelEffect(const Value: TLabelEffect);
    procedure SetRowPitch(const Value: TRowPitch);
    procedure SetFontEffect(const Value: TCnAAEffect);
  protected
    function IsLinesStored: Boolean; virtual;
    property Lines: TStrings read FLines write SetLines stored IsLinesStored;
    {* �ؼ��ı�����}
    property RowPitch: TRowPitch read FRowPitch write SetRowPitch default 20;
    {* �ı��м��}
    property LabelEffect: TLabelEffect read FLabelEffect write SetLabelEffect
      default leUntilNextLabel;
    {* �ı������塢���Ʊ�ǩ�����÷�Χ}
    property FontEffect: TCnAAEffect read FFontEffect write SetFontEffect;
    {* ƽ����Ч��������}
  public
    constructor Create(AOwner: TCnAAGraphicControl; ChangedProc: TNotifyEvent);
      override;
    {* �๹����}
    destructor Destroy; override;
    {* ��������}
    procedure Assign(Source: TPersistent); override;
    {* ����ֵ����}
  end;

{ TCnAAGraphicControl }

  TProgress = 0..csMaxProgress;

  TCnAAGraphicControl = class(TGraphicControl)
  {* ƽ������ؼ����࣬����ƽ������ؼ��ɸû�������������һ�㲻��Ҫ�û�ֱ�Ӵ���
   |<BR> ����û���Ҫ��д�Լ���ƽ������ؼ�������ϸ�����û���Դ��}
  private
{$IFNDEF COMPILER6_UP}
    FAutoSize: Boolean;
{$ENDIF}
    FAAFont: TCnAAFontEx;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FAutoUpdate: Boolean;
    FDrag: TCnDrag;
    FParentEffect: TCnParentEffect;
    FUpdateCount: Integer;
    FBorder: TBorderWidth;
    Inited: Boolean;
    Drawing: Boolean;
    AHeight: Integer;
    AWidth: Integer;

    procedure SetBorder(const Value: TBorderWidth);
    procedure SetDrag(const Value: TCnDrag);
    procedure SetParentEffect(const Value: TCnParentEffect);
    function GetWrapText(const Line, BreakStr: string;
      BreakChars: TSysCharSet; MaxCol: Integer): string;
    procedure SetAutoUpdate(const Value: Boolean);
  protected
{$IFDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); override;
{$ELSE}
    procedure SetAutoSize(const Value: Boolean); virtual;
{$ENDIF}
    procedure OnEffectChanged(Sender: TObject);
    procedure CopyParentImage(Dest: TCanvas);
    procedure WrapText(const S: string; Strs: TStrings; Col: Integer);
    procedure Blend(DesBmp, BkBmp, ForeBmp: TBitmap; AProgress: TProgress);
    procedure DrawTiled(Canvas: TCanvas; Rect: TRect; G: TGraphic);
    procedure DrawBackGround(Canvas: TCanvas; Rect: TRect; G: TGraphic;
      Mode: TBackGroundMode);
    procedure WndProc(var message: TMessage); override;
    procedure PaintCanvas; virtual;
    procedure Paint; override;
    procedure Loaded; override;
    procedure LoadedEx; virtual;
    procedure Reset; virtual;
    procedure Resize; override;
    property UpdateCount: Integer read FUpdateCount;
    property AAFont: TCnAAFontEx read FAAFont;
{$IFDEF COMPILER6_UP}
    property AutoSize default True;
{$ELSE}
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
{$ENDIF}
    property AutoUpdate: Boolean read FAutoUpdate write SetAutoUpdate default True;
    property Border: TBorderWidth read FBorder write SetBorder default 0;
    {* �ؼ��߽籣�����}
  public
    constructor Create(AOwner: TComponent); override;
    {* �๹����}
    destructor Destroy; override;
    {* ��������}
    property Canvas;
    {* �ؼ�����}
    procedure BeginUpdate;
    {* ��ʼ���£����ø÷����󣬶Կؼ����Եĸ��Ĳ��ᵼ�¿ؼ��ػ棬�����������޸�
       �ؼ�ʱʹ�á�
     |<BR> ע��÷��������� EndUpate �ɶ�ʹ�á�}
    procedure EndUpdate;
    {* �������£��� BeginUpdate���û��������º�ͨ����Ӧ���� Changed ����֪ͨ�ؼ��ػ档}
    procedure Changed;
    {* ֪ͨ�ؼ������ѱ����Ҫ��ؼ��ػ�}
  published
    property Drag: TCnDrag read FDrag write SetDrag;
    {* �϶�������Դ��}
    property ParentEffect: TCnParentEffect read FParentEffect write SetParentEffect;
    {* ���ؼ�Ӱ��������Դ��}
    property Align;
    property Anchors;
    property Constraints;
    property Enabled;
    property ShowHint;
    property Hint;
    property PopupMenu;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    {* �������ؼ��ڲ��¼�}
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    {* ����Ƴ��ؼ��ڲ��¼�}
    property OnStartDock;
    property OnStartDrag;
  end;

{ TCnAACustomText }

  TCnAACustomText = class(TCnAAGraphicControl)
  {* ƽ�������ı���ؼ����࣬����ʹ�ö����ı���ƽ������ؼ��ɸû�������������
     һ�㲻��Ҫ�û�ֱ�Ӵ�����
   |<BR> ����û���Ҫ��д�Լ���ƽ������ؼ����ɷ����û���Դ��}
  private
    FFonts: TCnFontLabels;
    FOnTextReady: TNotifyEvent;
    FOnComplete: TNotifyEvent;
    FOnPainted: TNotifyEvent;
    FLabels: TCnUserLabels;
    FLabelsInited: Boolean;
    FFontsInited: Boolean;
    procedure SetFonts(const Value: TCnFontLabels);
    procedure SetLabels(const Value: TCnUserLabels);
  protected
    procedure CreateDefLabels; virtual;
    procedure CreateDefFonts; virtual;
    procedure CreateDefault;
    function UseDefaultLabels: Boolean; virtual;
    procedure LoadedEx; override;
    procedure OnLabelChanged(Sender: TObject);
    property OnComplete: TNotifyEvent read FOnComplete write FOnComplete;
    property OnTextReady: TNotifyEvent read FOnTextReady write FOnTextReady;
    property OnPainted: TNotifyEvent read FOnPainted write FOnPainted;
  public
    constructor Create(AOwner: TComponent); override;
    {* �๹����}
    destructor Destroy; override;
    {* ��������}
  published
    property Fonts: TCnFontLabels read FFonts write SetFonts;
    {* �����ǩ����}
    property Labels: TCnUserLabels read FLabels write SetLabels;
    {* �û���ǩ����}
  end;

var
  HSLRange: Integer = 240;
  {* HSL��������ɫ�ķ�Χֵ}

// HSL��ɫ��RGBɫת������
function HSLtoRGB(H, S, L: Double): TColor;
{* HSL��ɫת��ΪRGB��ɫ
 |<PRE>
   H, S, L: Double      - �ֱ�Ϊɫ�������Ͷȡ����ȷ�����Ϊ"0"��"1"֮���С��
   Result: TColor       - ���� RGB ��ɫֵ
 |</PRE>}
function HSLRangeToRGB(H, S, L: Integer): TColor;
{* HSL��ɫת��ΪRGB��ɫ
 |<PRE>
   H, S, L: Integer     - �ֱ�Ϊɫ�������Ͷȡ����ȷ�����0..240
   Result: TColor       - ����RGB��ɫֵ
 |</PRE>}
procedure RGBtoHSL(RGB: TColor; var H, S, L: Double);
{* RGB��ɫת��ΪHSL��ɫ
 |<PRE>
   Color: TColor        - RGB��ɫֵ
   H, S, L: Integer     - ����ֱ�Ϊɫ�������Ͷȡ����ȷ�����Ϊ"0"��"1"֮���С��
 |</PRE>}
procedure RGBtoHSLRange(RGB: TColor; var H, S, L: Integer);
{* RGB��ɫת��ΪHSL��ɫ
 |<PRE>
   Color: TColor        - RGB��ɫֵ
   H, S, L: Integer     - ����ֱ�Ϊɫ�������Ͷȡ����ȷ�����0..240
 |</PRE>}

implementation

{$R-}
{$OVERFLOWCHECKS OFF}

const
  ItalicAdjust = 0.3;                   // б���ֿ��У��ϵ��
{$IFNDEF COMPILER6_UP}
  AC_SRC_ALPHA = $01;
{$ENDIF}

resourcestring
  SInvalidForeground = 'Invalid foreground bitmap!';
  SDuplicateString = 'Duplicate string!';
//  SAlreadyMultiplied = 'Already Multiplied!';
//  SAlreadyUnMultiplied = 'Already UnMultiplied!';

type
  PDWordArray = ^TDWordArray;
  TDWordArray = array[0..8192] of DWORD;
  {* DWORD ��������}

  TCnParentControl = class(TWinControl);
  TCnMyControl = class(TControl);

{$IFDEF FPC}

// FPC ��û�⺯�����ֹ�����
function CopyPalette(Palette: HPALETTE): HPALETTE;
var
  PaletteSize: Integer;
  LogPal: TMaxLogPalette;
begin
  Result := 0;
  if Palette = 0 then Exit;
  PaletteSize := 0;
  if GetObject(Palette, SizeOf(PaletteSize), @PaletteSize) = 0 then Exit;
  if PaletteSize = 0 then Exit;
  with LogPal do
  begin
    palVersion := $0300;
    palNumEntries := PaletteSize;
{$IFDEF FPC}
    GetPaletteEntries(Palette, 0, PaletteSize, @palPalEntry[0]);
{$ELSE}
    GetPaletteEntries(Palette, 0, PaletteSize, palPalEntry);
{$ENDIF}
  end;
  Result := CreatePalette(PLogPalette(@LogPal)^);
end;
{$ENDIF}

function HSLtoRGB(H, S, L: Double): TColor;
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
  R, G, B: Byte;
begin
  if S = 0 then
  begin
    R := Round(255 * L);
    G := R;
    B := R
  end
  else
  begin
    if L <= 0.5 then
      M2 := L * (1 + S)
    else
      M2 := L + S - L * S;
    M1 := 2 * L - M2;
    R := HueToColourValue(H + 1 / 3);
    G := HueToColourValue(H);
    B := HueToColourValue(H - 1 / 3)
  end;
  Result := RGB(R, G, B)
end;

function HSLRangeToRGB(H, S, L: Integer): TColor;
begin
  Result := HSLtoRGB(H / (HSLRange - 1), S / HSLRange, L / HSLRange)
end;

procedure RGBtoHSL(RGB: TColor; var H, S, L: Double);
  function Max(a, b: Double): Double;
  begin
    if a > b then
      Result := a
    else
      Result := b
  end;
  function Min(a, b: Double): Double;
  begin
    if a < b then
      Result := a
    else
      Result := b
  end;
var
  R, G, B, D, Cmax, Cmin: Double;
begin
  R := GetRValue(RGB) / 255;
  G := GetGValue(RGB) / 255;
  B := GetBValue(RGB) / 255;
  Cmax := Max(R, Max(G, B));
  Cmin := Min(R, Min(G, B));
  L := (Cmax + Cmin) / 2;
  if Cmax = Cmin then
  begin
    H := 0;
    S := 0
  end
  else
  begin
    D := Cmax - Cmin;
    if L < 0.5 then
      S := D / (Cmax + Cmin)
    else
      S := D / (2 - Cmax - Cmin);
    if R = Cmax then
      H := (G - B) / D
    else if G = Cmax then
      H := 2 + (B - R) / D
    else
      H := 4 + (R - G) / D;
    H := H / 6;
    if H < 0 then
      H := H + 1
  end
end;

procedure RGBtoHSLRange(RGB: TColor; var H, S, L: Integer);
var
  Hd, Sd, Ld: Double;
begin
  RGBtoHSL(RGB, Hd, Sd, Ld);
  H := Round(Hd * (HSLRange - 1));
  S := Round(Sd * HSLRange);
  L := Round(Ld * HSLRange);
end;

procedure StrectchDrawGraphic(ACanvas: TCanvas; ARect: TRect; AGraphic: TGraphic;
  BkColor: TColor);
var
  Bmp: TBitmap;
begin
  if AGraphic is TIcon then
  begin
    //  TIcon ��֧�����Ż��ƣ�ͨ�� TBitmap ��ת
    Bmp := TBitmap.Create;
    try
      Bmp.Canvas.Brush.Color := BkColor;
      Bmp.Canvas.Brush.Style := bsSolid;
      Bmp.Width := AGraphic.Width;
      Bmp.Height := AGraphic.Height;
      // Bmp.Canvas.FillRect(Rect(0, 0, Bmp.Width, Bmp.Height));
      Bmp.Canvas.Draw(0, 0, AGraphic);
      ACanvas.StretchDraw(ARect, Bmp);
    finally
      Bmp.Free;
    end;
  end
  else
    ACanvas.StretchDraw(ARect, AGraphic);
end;

procedure PreMultiplyBitmap(Bmp: TBitmap);
{$IFNDEF TGRAPHIC_SUPPORT_PARTIALTRANSPARENCY}
var
  W, H: Integer;
  Alpha: Word;
  PBGRA: PBGRAArray;
{$ENDIF}
begin
  if (Bmp = nil) or (Bmp.PixelFormat <> pf32bit) then
    Exit;

{$IFDEF TGRAPHIC_SUPPORT_PARTIALTRANSPARENCY}
  if Bmp.AlphaFormat <> afPremultiplied then
    Bmp.AlphaFormat := afPremultiplied;
//  else
//    raise Exception.Create(SAlreadyMultiplied);
{$ELSE}
  // �ֹ� PreMultiply
  for H := 0 to Bmp.Height - 1 do
  begin
    PBGRA := Bmp.ScanLine[H];
    for W := 0 to Bmp.Width - 1 do
    begin
      Alpha := PBGRA[W].rgbReserved;
      PBGRA[W].rgbBlue := MulDiv(PBGRA[W].rgbBlue, Alpha, 255);
      PBGRA[W].rgbGreen := MulDiv(PBGRA[W].rgbGreen, Alpha, 255);
      PBGRA[W].rgbRed := MulDiv(PBGRA[W].rgbRed, Alpha, 255);
    end;
  end;
{$ENDIF}
end;

procedure UnPreMultiplyBitmap(Bmp: TBitmap);
{$IFNDEF TGRAPHIC_SUPPORT_PARTIALTRANSPARENCY}
var
  W, H: Integer;
  Alpha: Word;
  PBGRA: PBGRAArray;
{$ENDIF}
begin
  if (Bmp = nil) or (Bmp.PixelFormat <> pf32bit) then
    Exit;

{$IFDEF TGRAPHIC_SUPPORT_PARTIALTRANSPARENCY}
  if Bmp.AlphaFormat = afPremultiplied then
    Bmp.AlphaFormat := afIgnored;
//  else
//    raise Exception.Create(SAlreadyUnMultiplied);
{$ELSE}
  // �ֹ� UnPreMultiply
  for H := 0 to Bmp.Height - 1 do
  begin
    PBGRA := Bmp.ScanLine[H];
    for W := 0 to Bmp.Width - 1 do
    begin
      Alpha := PBGRA[W].rgbReserved;
      PBGRA[W].rgbBlue := MulDiv(PBGRA[W].rgbBlue, 255, Alpha);
      PBGRA[W].rgbGreen := MulDiv(PBGRA[W].rgbGreen, 255, Alpha);
      PBGRA[W].rgbRed := MulDiv(PBGRA[W].rgbRed, 255, Alpha);
    end;
  end;
{$ENDIF}
end;

type
  TLogPal = record
    lpal: TLogPalette;
    dummy: array[0..255] of TPaletteEntry;
  end;

var
  HGrayPal: HPALETTE = 0;
  LogPal: TLogPal;

// ��ʼ���Ҷ�λͼ
procedure InitGrayPal;
var
  I: Integer;
begin
  LogPal.lpal.palVersion := $300;
  LogPal.lpal.palNumEntries := 256;
  for I := 0 to 255 do
  begin
    LogPal.dummy[I].peRed := I;
    LogPal.dummy[I].peGreen := I;
    LogPal.dummy[I].peBlue := I;
    LogPal.dummy[I].peFlags := 0;
  end;
  HGrayPal := CreatePalette(LogPal.lpal);
end;

{ TCnAAMask }

//--------------------------------------------------------//
// ƽ�������ɰ���                                         //
//--------------------------------------------------------//

// ��ֵ
procedure TCnAAMask.Assign(Source: TPersistent);
begin
  if Source is TCnAAMask then
  begin
    FWidth := TCnAAMask(Source).Width;
    FHeight := TCnAAMask(Source).Height;
    Quality := TCnAAMask(Source).Quality;
    BytesLineGray := TCnAAMask(Source).BytesLineGray;
    BytesLineMask := TCnAAMask(Source).BytesLineMask;
    ReAllocMem(FPMaskBuff, FHeight * BytesLineMask);
    CopyMemory(FPMaskBuff, TCnAAMask(Source).FPMaskBuff, FHeight * BytesLineMask);
  end
  else
  begin
    inherited Assign(Source);
  end;
end;

// ��ʼ��
constructor TCnAAMask.Create(AOwner: TCnAAFont);
begin
  FAAFont := AOwner;
  FPMaskBuff := nil;
  Quality := aqNormal;
end;

// �ͷ�
destructor TCnAAMask.Destroy;
begin
  FreeGrayBmp;
  FreeMem(FPMaskBuff);
  inherited;
end;

procedure TCnAAMask.InitGrayBmp;
begin
  if FGrayBmp = nil then
  begin
    FGrayBmp := TBitmap.Create;
    FGrayBmp.PixelFormat := pf8bit;
    FGrayBmp.Canvas.Brush.Style := bsSolid;
    FGrayBmp.Canvas.Brush.Color := clBlack;
    FGrayBmp.Palette := CopyPalette(HGrayPal);
  end;
end;

procedure TCnAAMask.FreeGrayBmp;
var
  P: HPALETTE;
begin
  if FGrayBmp <> nil then
  begin
    P := FGrayBmp.Palette;
    FGrayBmp.Palette := 0;
    FreeAndNil(FGrayBmp);
    DeleteObject(P);
  end;
end;

// ����ƽ�������ɰ�
procedure TCnAAMask.DrawMaskEx(const Text: string; Extend: TSize; Point: TPoint);
var
  I, J: Integer;
  pS1, pS2, pS3, pS4: PByteArray;
  pDes: PByteArray;
  X, Y: Integer;
  P: TPoint;
  LogFont: TLogFont;
  Beta: Double;
  TextSize: TSize;
  R: TRect;
begin
  if (FAAFont = nil) or (FAAFont.Canvas = nil) then
    Exit;

  InitGrayBmp;
  FWidth := Extend.cx;                  // ��С
  FHeight := Extend.cy;
  if FGrayBmp.Width < Width * Scale then // �Ŵ�
    FGrayBmp.Width := Width * Scale;
  if FGrayBmp.Height < Height * Scale then
    FGrayBmp.Height := Height * Scale;

  GetObject(FAAFont.Canvas.Font.Handle, SizeOf(TLogFont), @LogFont);
  with LogFont do
  begin
    lfHeight := lfHeight * Scale;
    lfWidth := lfWidth * Scale;
    Beta := lfEscapement * Pi / 1800;
  end;

  FGrayBmp.Canvas.Font.Handle := CreateFontIndirect(LogFont);
  FGrayBmp.Canvas.Font.Color := clWhite;
  FillRect(FGrayBmp.Canvas.Handle, Bounds(0, 0, FGrayBmp.Width, FGrayBmp.Height), 0);
  X := Point.X * Scale;
  Y := Point.Y * Scale;
  if Beta <> 0 then      // ����������ת
  begin
    TextSize := TextExtentEx(Text, P);
    Inc(X, P.X * Scale);
    Inc(Y, P.Y * Scale);
  end;
  R := Bounds(0, 0, FGrayBmp.Width, FGrayBmp.Height);
  Windows.TextOut(FGrayBmp.Canvas.Handle, X, Y, PChar(Text), Length(Text));

  BytesLineGray := (FGrayBmp.Width + 3) div 4 * 4; // ɨ���߿��
  BytesLineMask := (Width + 3) div 4 * 4;
  ReAllocMem(FPMaskBuff, BytesLineMask * Height);

  pS1 := FGrayBmp.ScanLine[0];           // Դ�Ҷ�ͼ
  pS2 := PByteArray(TCnNativeInt(pS1) - BytesLineGray);
  pS3 := PByteArray(TCnNativeInt(pS2) - BytesLineGray);
  pS4 := PByteArray(TCnNativeInt(pS3) - BytesLineGray);
  pDes := PByteArray(TCnNativeInt(pMaskBuff) + (Height - 1) * BytesLineMask);
  // Ŀ��Ҷ�ΪԴ���ο��ƽ��ֵ

  case Quality of
    aqHigh:
      begin                             // �߾��� 4X4 ����
        for I := 0 to Height - 1 do
        begin
          for J := 0 to Width - 1 do
          begin
            X := J * 4;
            pDes^[J] :=
              (pS1^[X] + pS1^[X + 1] + pS1^[X + 2] + pS1^[X + 3] +
              pS2^[X] + pS2^[X + 1] + pS2^[X + 2] + pS2^[X + 3] +
              pS3^[X] + pS3^[X + 1] + pS3^[X + 2] + pS3^[X + 3] +
              pS4^[X] + pS4^[X + 1] + pS4^[X + 2] + pS4^[X + 3]) shr 4;
          end;
          pS1 := PByteArray(TCnNativeInt(pS4) - BytesLineGray);
          pS2 := PByteArray(TCnNativeInt(pS1) - BytesLineGray);
          pS3 := PByteArray(TCnNativeInt(pS2) - BytesLineGray);
          pS4 := PByteArray(TCnNativeInt(pS3) - BytesLineGray);
          pDes := PByteArray(TCnNativeInt(pDes) - BytesLineMask);
        end;
      end;
    aqNormal:
      begin                             // ��ͨ���� 3X3 ����
        for I := 0 to Height - 1 do
        begin
          for J := 0 to Width - 1 do
          begin
            X := J * 3;
            pDes^[J] :=
              (pS1^[X] + pS1^[X + 1] + pS1^[X + 2] shr 1 +
              pS2^[X] + pS2^[X + 1] + pS2^[X + 2] +
              pS3^[X] shr 1 + pS3^[X + 1] + pS3^[X + 2]) shr 3;
          end;
          pS1 := PByteArray(TCnNativeInt(pS3) - BytesLineGray);
          pS2 := PByteArray(TCnNativeInt(pS1) - BytesLineGray);
          pS3 := PByteArray(TCnNativeInt(pS2) - BytesLineGray);
          pDes := PByteArray(TCnNativeInt(pDes) - BytesLineMask);
        end;
      end;
    aqLow:
      begin                             // �;��� 2X2 ����
        for I := 0 to Height - 1 do
        begin
          for J := 0 to Width - 1 do
          begin
            X := J * 2;
            pDes^[J] :=
              (pS1^[X] + pS1^[X + 1] +
              pS2^[X] + pS2^[X + 1]) shr 2;
          end;
          pS1 := PByteArray(TCnNativeInt(pS2) - BytesLineGray);
          pS2 := PByteArray(TCnNativeInt(pS1) - BytesLineGray);
          pDes := PByteArray(TCnNativeInt(pDes) - BytesLineMask);
        end;
      end;
    aqNone:
      begin                             // ��ƽ��Ч��
        for I := 0 to Height - 1 do
        begin
          CopyMemory(pDes, pS1, Width);
          pS1 := PByteArray(TCnNativeInt(pS1) - BytesLineGray);
          pDes := PByteArray(TCnNativeInt(pDes) - BytesLineMask);
        end;
      end;
  end;
  FreeGrayBmp;
end;

// ����ƽ������
procedure TCnAAMask.DrawMask(const Text: string);
begin
  DrawMaskEx(Text, TextExtent(Text), Point(0, 0));
end;

// ��Ե���
procedure TCnAAMask.Outline;
var
  X, Y: Integer;
  s1, s2, s3, s4, Sum: Integer;
  pTempBuff: PByteArray;
  pDes: PByteArray;
  pUp, pMiddle, pDown: PByteArray;      // �����ָ��
begin
  GetMem(pTempBuff, BytesLineMask * Height); // ��ʱ������
  try
    CopyMemory(pTempBuff, pMaskBuff, BytesLineMask * Height);
    for Y := 1 to Height - 2 do
    begin
      pUp := ScanLine(Y - 1, pTempBuff);
      pMiddle := ScanLine(Y, pTempBuff);
      pDown := ScanLine(Y + 1, pTempBuff);
      pDes := ScanLine(Y);
      for X := 1 to Width - 2 do
      begin
        s1 := Abs(pDown^[X] - pUp^[X]);
        s2 := Abs(pMiddle^[X + 1] - pMiddle^[X - 1]);
        s3 := Abs(pDown^[X - 1] - pUp^[X + 1]);
        s4 := Abs(pDown^[X + 1] - pUp^[X - 1]);
        Sum := (s1 + s2 + s3 + s4) shr 2;
        if Sum > 255 then
          pDes^[X] := 255
        else
          pDes^[X] := Sum;
      end;
    end;
  finally
    FreeMem(pTempBuff);
  end;
end;

// ����ģ��
procedure TCnAAMask.Blur(Blur: TBlurStrength);
type
  TLine = array[0..4] of Integer;
const
  csLine: array[0..4] of TLine = (
    (0, 0, 0, 1, 2), (-1, -1, 0, 1, 2), (-2, -1, 0, 1, 2),
    (-2, -1, 0, 1, 1), (-2, -1, 0, 0, 0)); // �߽紦����
var
  pTempBuff: PByteArray;
  pSour: array[0..4] of PByteArray;
  pDes: PByteArray;
  xLine: TLine;
  yLine: TLine;
  X, Y, I: Integer;
  Sum: Integer;
  ABlur: Byte;
begin
  GetMem(pTempBuff, BytesLineMask * Height); // ��ʱ������
  try
    CopyMemory(pTempBuff, pMaskBuff, BytesLineMask * Height);
    ABlur := Round(Blur * 255 / 100);
    for Y := 0 to Height - 1 do         // �߽紦��
    begin
      if Y = 0 then
        yLine := csLine[0]
      else if Y = 1 then
        yLine := csLine[1]
      else if Y = Height - 2 then
        yLine := csLine[3]
      else if Y = Height - 1 then
        yLine := csLine[4]
      else
        yLine := csLine[2];
      for I := 0 to 4 do
        pSour[I] := ScanLine(yLine[I] + Y, pTempBuff);
      pDes := ScanLine(Y, pMaskBuff);
      for X := 0 to Width - 1 do        // �߽紦��
      begin
        if X = 0 then
          xLine := csLine[0]
        else if X = 1 then
          xLine := csLine[1]
        else if X = Width - 2 then
          xLine := csLine[3]
        else if X = Width - 1 then
          xLine := csLine[4]
        else
          xLine := csLine[2];
        Sum := 0;
        for I := 0 to 4 do              // 5X5��ֵ����
          Inc(Sum, pSour[I]^[X + xLine[0]] + pSour[I]^[X + xLine[1]] +
            pSour[I]^[X + xLine[2]] + pSour[I]^[X + xLine[3]] +
            pSour[I]^[X + xLine[3]]);
        if ABlur = 255 then             // ģ����
          pDes^[X] := Round(Sum / 25)
        else
          pDes^[X] := (Round(Sum / 25) - pDes^[X]) * ABlur shr 8 + pDes^[X];
      end;
    end;
  finally
    FreeMem(pTempBuff);
  end;
end;

//  �罦Ч��
procedure TCnAAMask.Spray(Amount: Integer);
var
  r, X, Y, ax, ay: Integer;
  pDes: PByteArray;
begin
  pDes := ScanLine(0);
  for Y := 0 to FHeight - 1 do
  begin
    for X := 0 to FWidth - 1 do
    begin
      r := Random(Amount + 1);
      ax := X + r - Random(r * 2);
      if ax < 0 then
        ax := 0
      else if ax > FWidth - 1 then
        ax := FWidth - 1;
      ay := Y + r - Random(r * 2);
      if ay < 0 then
        ay := 0
      else if ay > FHeight - 1 then
        ay := FHeight - 1;
      pDes^[X] := PByteArray(ScanLine(ay))[ax];
    end;
    pDes := PByteArray(TCnNativeInt(pDes) - BytesLineMask);
  end;
end;

// ���ɰ�ͼ����ˮƽ������
procedure TCnAAMask.HorzMirror;
var
  X, Y: Integer;
  c: Byte;
  pLine: PByteArray;
begin
  for Y := 0 to FHeight - 1 do
  begin
    pLine := ScanLine(Y);
    for X := 0 to FWidth div 2 - 1 do
    begin
      c := pLine[X];
      pLine[X] := pLine[FWidth - 1 - X];
      pLine[FWidth - 1 - X] := c;
    end;
  end;
end;

// ���ɰ�ͼ���д�ֱ������
procedure TCnAAMask.VertMirror;
var
  pSrc, pDst, pBuf: PByteArray;
  I: Integer;
begin
  GetMem(pBuf, BytesLineMask);
  try
    for I := 0 to FHeight div 2 - 1 do
    begin
      pSrc := ScanLine(I);
      pDst := ScanLine(FHeight - 1 - I);
      CopyMemory(pBuf, pSrc, BytesLineMask);
      CopyMemory(pSrc, pDst, BytesLineMask);
      CopyMemory(pDst, pBuf, BytesLineMask);
    end;
  finally
    FreeMem(pBuf);
  end;
end;

// ���ص�ַ
function TCnAAMask.PixelAddr(X, Y: Integer): Pointer;
begin
  if (X < 0) or (X > Width - 1) or (Y < 0) or (Y > Height - 1) then
    raise EInvalidPixel.Create('Invalid pixel!')
  else
    Result := Pointer(TCnNativeInt(FPMaskBuff) + (Height - 1 + Y) * BytesLineMask + X);
end;

// ����
function TCnAAMask.Pixel(X, Y: Integer): Byte;
begin
  if (X < 0) or (X > Width - 1) or (Y < 0) or (Y > Height - 1) then
    raise EInvalidPixel.Create('Invalid pixel!')
  else
    Result := PByteArray(TCnNativeInt(FPMaskBuff) + (Height - 1 + Y) * BytesLineMask)[X];
end;

// ɨ���ߵ�ַ
function TCnAAMask.ScanLine(Line: Integer): Pointer;
begin
  if (Line < 0) or (Line > Height - 1) then
    raise EInvalidLine.Create('Invalid line!')
  else
    Result := Pointer(TCnNativeInt(FPMaskBuff) + (Height - 1 - Line) * BytesLineMask);
end;

function TCnAAMask.ScanLine(Line: Integer; pAData: PByteArray): PByteArray;
begin
  Result := PByteArray(TCnNativeInt(pAData) + (Height - 1 - Line) * BytesLineMask);
end;

// ���þ���
procedure TCnAAMask.SetQuality(const Value: TAAQuality);
begin
  FQuality := Value;
  case FQuality of
    aqHigh: Scale := 4;
    aqNormal: Scale := 3;
    aqLow: Scale := 2;
    aqNone: Scale := 1;
  else
    Scale := 1;
  end;
end;

function GetRotateSize(Size: TSize; Angle: Double; var StartPoint: TPoint): TSize;
var
  p1, p2, p3, p4: TPoint;
  cAngle, sAngle: Double;
  wCos, hCos, wSin, hSin: Double;
  SrcW2, SrcH2: Double;
  Rect: TRect;
begin
  sAngle := Sin(-Angle);
  cAngle := Cos(-Angle);

  // ����Ŀ�궥��λ��
  SrcW2 := Size.cx / 2;
  SrcH2 := Size.cy / 2;
  wCos := SrcW2 * cAngle;
  hCos := SrcH2 * cAngle;
  wSin := SrcW2 * sAngle;
  hSin := SrcH2 * sAngle;
  p1.X := Round(-wCos + hSin); // ����
  p1.Y := Round(-wSin - hCos);
  p2.X := Round(wCos + hSin); // ����
  p2.Y := Round(wSin - hCos);
  p3.X := Round(-wCos - hSin); // ����
  p3.Y := Round(-wSin + hCos);
  p4.X := Round(wCos - hSin); // ����
  p4.Y := Round(wSin + hCos);

  // �����������
  Rect.Left := MinIntValue([p1.X, p2.X, p3.X, p4.X]);
  Rect.Right := MaxIntValue([p1.X, p2.X, p3.X, p4.X]);
  Rect.Top := MinIntValue([p1.Y, p2.Y, p3.Y, p4.Y]);
  Rect.Bottom := MaxIntValue([p1.Y, p2.Y, p3.Y, p4.Y]);

  Result.cx := Rect.Right - Rect.Left;
  Result.cy := Rect.Bottom - Rect.Top;
  StartPoint.X := p1.X + Result.cx div 2;
  StartPoint.Y := p1.Y + Result.cy div 2;
end;

// �ı��ߡ�����ת����ʼλ��
function TCnAAMask.TextExtentEx(const S: string; var Point: TPoint): TSize;
var
  LogFont: TLogFont;
  TempFont, SaveFont: HFONT;
  DC: HDC;
  Beta: Double;
begin
  Result.cx := 0;
  Result.cy := 0;
  if (FAAFont = nil) or (FAAFont.Canvas = nil) then
    Exit;

  DC := GetDC(0);
  try
    GetObject(FAAFont.Canvas.Font.Handle, SizeOf(TLogFont), @LogFont);
    with LogFont do
    begin
      lfHeight := lfHeight * Scale;
      lfWidth := lfWidth * Scale;
      Beta := lfEscapement * Pi / 1800;
    end;
    TempFont := CreateFontIndirect(LogFont);
    try
      SaveFont := SelectObject(DC, TempFont);
      GetTextExtentPoint32(DC, PChar(S), Length(S), Result);
      Result.cx := (Result.cx + Scale - 1) div Scale;
      Result.cy := (Result.cy + Scale - 1) div Scale;
      if (fsItalic in FAAFont.Canvas.Font.Style) and (Length(S) > 0) then
        Result.cx := Result.cx + Round(Result.cx / Length(S) * ItalicAdjust);
      SelectObject(DC, SaveFont);
      if Beta <> 0 then
      begin
        Result := GetRotateSize(Result, Beta, Point);
      end;
    finally
      DeleteObject(TempFont);
    end;
  finally
    ReleaseDC(0, DC);
  end;
end;

// �ı��ߡ���
function TCnAAMask.TextExtent(const S: string): TSize;
var
  Point: TPoint;
begin
  Result := TextExtentEx(S, Point);
end;

// �ı��߶�
function TCnAAMask.TextHeight(const S: string): Integer;
begin
  Result := TextExtent(S).cy;
end;

// �ı����
function TCnAAMask.TextWidth(const S: string): Integer;
begin
  Result := TextExtent(S).cx;
end;

{ TCnAABlend }

//--------------------------------------------------------//
// ǰ�������ɰ�����                                     //
//--------------------------------------------------------//

// ��ʼ��
constructor TCnAABlend.Create(AOwner: TCnAAFont);
begin
  FAAFont := AOwner;
  FForeBmp := TBitmap.Create;
  FForeBmp.PixelFormat := pf24bit;
  FRGBBmp := TBitmap.Create;
  FRGBBmp.PixelFormat := pf24bit;

  FForeBmp32 := TBitmap.Create;
  FForeBmp32.PixelFormat := pf32bit;
{$IFDEF TGRAPHIC_SUPPORT_PARTIALTRANSPARENCY}
  FForeBmp32.AlphaFormat := afIgnored;
{$ENDIF}

  FBGRABmp := TBitmap.Create;
  FBGRABmp.PixelFormat := pf32bit;
{$IFDEF TGRAPHIC_SUPPORT_PARTIALTRANSPARENCY}
  FBGRABmp.AlphaFormat := afIgnored;
{$ENDIF}
end;

// �ͷ�
destructor TCnAABlend.Destroy;
begin
  ForeBmp.Free;
  FForeBmp32.Free;
  FRGBBmp.Free;
  FBGRABmp.Free;
  inherited;
end;

// ��ֵ
procedure TCnAABlend.Assign(Source: TPersistent);
begin
  if Source is TCnAABlend then
  begin
    ForeBmp.Assign(TCnAABlend(Source).ForeBmp);
    ForeBmp32.Assign(TCnAABlend(Source).ForeBmp32);
  end
  else
    inherited Assign(Source);
end;

procedure TCnAABlend.SyncForeBmp32;
var
  Bf: TBlendFunction;
begin
  if FForeBmp.Empty then
    Exit;

  FForeBmp32.Width := FForeBmp.Width;
  FForeBmp32.Height := FForeBmp.Height;

  // ȫ�̸��ƹ�ȥ
  Bf.BlendOp := AC_SRC_OVER;
  Bf.BlendFlags := 0;
  Bf.SourceConstantAlpha := 255;
  Bf.AlphaFormat := 0;  // ������͸���Ȼ�ϣ����븴��

  AlphaBlend(FForeBmp32.Canvas.Handle, 0, 0, FForeBmp.Width, FForeBmp.Height,
    FForeBmp.Canvas.Handle, 0, 0, FForeBmp.Width, FForeBmp.Height, Bf);
end;

// �ı���ǰ��ɫ�뱳�����
procedure TCnAABlend.Blend(X, Y: Integer; AColor: TColor; Alpha: TAlpha;
  Mask: TCnAAMask; DestIsAlpha: Boolean);
var
  R, B, G: Byte;
  AAlpha, DiffForeAlpha: DWORD;
  ForeAlpha, BkAlpha: Byte;
  pMask: PByteArray;
  pRGB: PRGBArray;
  pBGRA: PBGRAArray;
  PDW: PDWordArray;
  Weight: Byte;
  I, J: Integer;
  Color, BColor: TColor;
  Color32Rec: TRGBQuad;
  Bf: TBlendFunction;
begin
  if (FAAFont = nil) or (FAAFont.Canvas = nil) then
    Exit;

  Color := ColorToRGB(AColor);          // ʵ��ǰ��ɫ
  R := GetRValue(Color);                // ɫ�ʷ���
  G := GetGValue(Color);
  B := GetBValue(Color);
  AAlpha := Alpha * $100 div 100;       // �õ�ǰ��͸��������ϵ����0 �� 256 ��Χ��

  // ���Ŀ���� 32 λ�� Alpha ͨ������Ҫ Alpha ֧�֣���ʹ�� 32 λ�� Alpha ���м�ͼ���
  // �� Alpha �����Ϻ���ͨ�� AlphaBlend ����͸�������ݹ�ȥ
  if DestIsAlpha then
  begin
    FBGRABmp.Width := Mask.Width;
    FBGRABmp.Height := Mask.Height;
{$IFDEF TGRAPHIC_SUPPORT_PARTIALTRANSPARENCY}
    FBGRABmp.AlphaFormat := afPremultiplied;
{$ENDIF}

    FBGRABmp.Canvas.Brush.Assign(FAAFont.Canvas.Brush);
    if FBGRABmp.Canvas.Brush.Style <> bsSolid then
    begin
      J := FBGRABmp.Width * SizeOf(TRGBQuad);
      for I := 0 to FBGRABmp.Height - 1 do
        FillChar(FBGRABmp.ScanLine[I]^, J, $00); // �����ȫ͸��������Ĭ�ϻ��ɫ���

      // ͸�����ȴ�͸���ȵظ���Ŀ�걳���� FBGRABmp
      Bf.BlendOp := AC_SRC_OVER;
      Bf.BlendFlags := 0;
      Bf.SourceConstantAlpha := $FF;
      Bf.AlphaFormat := AC_SRC_ALPHA;

      AlphaBlend(FBGRABmp.Canvas.Handle, 0, 0, FBGRABmp.Width, FBGRABmp.Height,
        FAAFont.Canvas.Handle, X, Y, FBGRABmp.Width, FBGRABmp.Height, Bf);

      // UnPremutiply �����������ĵĻ��
      UnPreMultiplyBitmap(FBGRABmp);
    end
    else
    begin
      // Solid ʱ���� FBGRABmp.Canvas.Brush.Color �����ȫ��͸��
      // FBGRABmp.Canvas.FillRect(Bounds(0, 0, FBGRABmp.Width, FBGRABmp.Height)); ��Ч�����ֹ�

      BColor := ColorToRGB(FBGRABmp.Canvas.Brush.Color);     // ʵ�ʱ���ɫ
      Color32Rec.rgbRed := GetRValue(BColor);                // ɫ�ʷ���
      Color32Rec.rgbGreen := GetGValue(BColor);
      Color32Rec.rgbBlue := GetBValue(BColor);
      Color32Rec.rgbReserved := $FF;

      for I := 0 to FBGRABmp.Height - 1 do
      begin
        PDW := FBGRABmp.ScanLine[I];
        for J := 0 to FBGRABmp.Width - 1 do
          PDW^[J] := PDWORD(@Color32Rec)^; // ֱ�ӷ����ڴ���ɫ���Ѿ��� UnPreMultiply ����
      end;
    end;

    for J := 0 to FBGRABmp.Height - 1 do
    begin
      // ����ÿһ�����ص� UnPreMultiply ��ǰ����������ɫ��Ϲ�ʽ��
      // Ŀ��͸����=1-��1-ǰ͸���ȣ�*��1-��͸���ȣ�
      // Ŀ�����=��ǰ����*ǰ͸���� + �����*��͸����*��1-ǰ͸���ȣ���/Ŀ��͸����
      // ������͸������ 0 �� 1 �������ڣ�1 ��ʾ��͸����Ҫ��취ת���� 0 �� 255

      pMask := Mask.ScanLine(J);
      pBGRA := FBGRABmp.ScanLine[J];
      for I := 0 to FBGRABmp.Width - 1 do
      begin
        // pMask[i] �� 0 �� 255 ��Χ�ڵ�����ǰ͸���ȣ���Ҫ * AAlpha shr 8 �õ�ʵ��ǰ͸����
        // pBGRA^[i].rgbReserved �� 0 �� 255 ��Χ�ڵĺ�͸����
        // ����Ŀ��͸���� = (65536 - (255 - ForeAlpha)(255 - BkAlpha)) / 255
        ForeAlpha := pMask[I] * AAlpha shr 8;
        BkAlpha := pBGRA^[I].rgbReserved;
        DiffForeAlpha := ($FF - ForeAlpha) * BkAlpha;

        // Weight �õ�Ŀ��͸����
        Weight := ($FF * $FF - ($FF - ForeAlpha) * ($FF - BkAlpha)) div $FF;
        // ��ǰ����ȫ��͸��Ҳ���� ForeAlpha Ϊ 255 ʱ�����۱���͸���ȶ��٣����͸����Ϊ 255��Ҳ������ȫ��͸��
        // ��ǰ����ȫ͸��Ҳ���� ForeAlpha Ϊ 0 ʱ�����͸����Ϊ����͸���� BkAlpha

        pBGRA^[I].rgbReserved := Weight;
        if Weight <> 0 then // 0 ��ʾȫ͸�����Ͳ���Ҫ��ɫ��
        begin
          pBGRA^[I].rgbBlue := (B * ForeAlpha + (pBGRA^[I].rgbBlue * DiffForeAlpha) div $FF) div Weight;
          pBGRA^[I].rgbGreen := (G * ForeAlpha + (pBGRA^[I].rgbGreen * DiffForeAlpha) div $FF) div Weight;
          pBGRA^[I].rgbRed := (R * ForeAlpha + (pBGRA^[I].rgbRed * DiffForeAlpha) div $FF) div Weight;
        end;
      end;
    end;

    PreMultiplyBitmap(FBGRABmp); // ����Ҫ������ PreMultiply �������������

    // ע��˴� FBGRABmp ���Ѿ������ FAAFont ԭʼ�����ˣ�
    // �����ٴκ� FAAFont �����ٴ�͸���Ȼ�ϣ�����ȫ�̸��ƹ�ȥ
    Bf.BlendOp := AC_SRC_OVER;
    Bf.BlendFlags := 0;
    Bf.SourceConstantAlpha := 255;
    Bf.AlphaFormat := 0;  // �����ٴ���͸���Ȼ���ˣ����븴��

    AlphaBlend(FAAFont.Canvas.Handle, X, Y, FBGRABmp.Width, FBGRABmp.Height,
      FBGRABmp.Canvas.Handle, 0, 0, FBGRABmp.Width, FBGRABmp.Height, Bf); // ���
  end
  else // ���Ŀ�겻��Ҫ Alpha ֧�֣�����Ŀ���� 24 λ���� 32 λ������ 24 λ�м�ͼ���� BitBlt ��ȥ
  begin
    FRGBBmp.Width := Mask.Width;
    FRGBBmp.Height := Mask.Height;
    FRGBBmp.Canvas.Brush.Assign(FAAFont.Canvas.Brush);
    if FRGBBmp.Canvas.Brush.Style <> bsSolid then
      BitBlt(FRGBBmp.Canvas.Handle, 0, 0, FRGBBmp.Width, FRGBBmp.Height,
        FAAFont.Canvas.Handle, X, Y, SRCCOPY) // ͸��
    else
      FillRect(FRGBBmp.Canvas.Handle, Bounds(0, 0, FRGBBmp.Width, FRGBBmp.Height), 0);

    for J := 0 to FRGBBmp.Height - 1 do
    begin
      pMask := Mask.ScanLine(J);
      pRGB := FRGBBmp.ScanLine[J];
      for I := 0 to FRGBBmp.Width - 1 do
      begin
        Weight := pMask^[I] * AAlpha shr 8; // ���ϵ��
        if Weight <> 0 then
        begin
          if Weight = 255 then
          begin                           // ǰ��ɫ
            pRGB^[I].rgbtBlue := B;
            pRGB^[I].rgbtGreen := G;
            pRGB^[I].rgbtRed := R;
          end
          else
          begin                           // ���
            Inc(pRGB^[I].rgbtBlue, Weight * (B - pRGB^[I].rgbtBlue) shr 8);
            Inc(pRGB^[I].rgbtGreen, Weight * (G - pRGB^[I].rgbtGreen) shr 8);
            Inc(pRGB^[I].rgbtRed, Weight * (R - pRGB^[I].rgbtRed) shr 8);
          end;
        end;
      end;
    end;

    BitBlt(FAAFont.Canvas.Handle, X, Y, FRGBBmp.Width, FRGBBmp.Height,
      FRGBBmp.Canvas.Handle, 0, 0, SRCCOPY); // ���
  end;
end;

// �ı��������뱳�����
procedure TCnAABlend.BlendEx(X, Y: Integer; Alpha: TAlpha; Mask: TCnAAMask;
  DestIsAlpha: Boolean);
var
  AAlpha, DiffForeAlpha: DWORD;
  ForeAlpha, BkAlpha: Byte;
  pMask: PByteArray;
  pRGB: PRGBArray;
  pFore: PRGBArray;
  pFor32: PBGRAArray;
  pBGRA: PBGRAArray;
  Weight: Byte;
  I, J: Integer;
  Bf: TBlendFunction;
  PDW: PDWordArray;
  BColor: TColor;
  Color32Rec: TRGBQuad;
begin
  if (FAAFont = nil) or (FAAFont.Canvas = nil) then
    Exit;

  if DestIsAlpha then
  begin
    if (ForeBmp32.Width <> Mask.Width) or (ForeBmp32.Height <> Mask.Height)
      or (ForeBmp32.PixelFormat <> pf32bit) then
    begin                                 // ���������ͼ
      raise EInvalidForeBmp.Create(SInvalidForeground);
      Exit;
    end;

    FBGRABmp.Width := Mask.Width;
    FBGRABmp.Height := Mask.Height;
{$IFDEF TGRAPHIC_SUPPORT_PARTIALTRANSPARENCY}
    FBGRABmp.AlphaFormat := afPremultiplied;
{$ENDIF}

    AAlpha := Alpha * $100 div 100;       // ͸����
    FBGRABmp.Canvas.Brush.Assign(FAAFont.Canvas.Brush);

    if FBGRABmp.Canvas.Brush.Style <> bsSolid then
    begin
      J := FBGRABmp.Width * SizeOf(TRGBQuad);
      for I := 0 to FBGRABmp.Height - 1 do
        FillChar(FBGRABmp.ScanLine[I]^, J, $00); // �����ȫ͸��������Ĭ�ϻ��ɫ���

      // ͸�����ȴ�͸���ȵظ���Ŀ�걳���� FBGRABmp
      Bf.BlendOp := AC_SRC_OVER;
      Bf.BlendFlags := 0;
      Bf.SourceConstantAlpha := $FF;
      Bf.AlphaFormat := AC_SRC_ALPHA;

      AlphaBlend(FBGRABmp.Canvas.Handle, 0, 0, FBGRABmp.Width, FBGRABmp.Height,
        FAAFont.Canvas.Handle, X, Y, FBGRABmp.Width, FBGRABmp.Height, Bf);

      // UnPremutiply �����������ĵĻ��
      UnPreMultiplyBitmap(FBGRABmp);
    end
    else
    begin
      // Solid ʱ���� FBGRABmp.Canvas.Brush.Color �����ȫ��͸��
      BColor := ColorToRGB(FBGRABmp.Canvas.Brush.Color);     // ʵ�ʱ���ɫ
      Color32Rec.rgbRed := GetRValue(BColor);                // ɫ�ʷ���
      Color32Rec.rgbGreen := GetGValue(BColor);
      Color32Rec.rgbBlue := GetBValue(BColor);
      Color32Rec.rgbReserved := $FF;

      for I := 0 to FBGRABmp.Height - 1 do
      begin
        PDW := FBGRABmp.ScanLine[I];
        for J := 0 to FBGRABmp.Width - 1 do
          PDW^[J] := PDWORD(@Color32Rec)^; // ֱ�ӷ����ڴ���ɫ���Ѿ��� UnPreMultiply ����
      end;
    end;

    // �ڲ�֧�� AlphaFormat �İ汾�У�ForeBmp32 ����������籣֤ PreMultiply���ڴ˴����� UnPreMultiply
{$IFDEF TGRAPHIC_SUPPORT_PARTIALTRANSPARENCY}
    ForeBmp32.AlphaFormat := afIgnored;
{$ENDIF}

    for J := 0 to FBGRABmp.Height - 1 do
    begin
      pMask := Mask.ScanLine(J);
      pBGRA := FBGRABmp.ScanLine[J];
      pFor32 := ForeBmp32.ScanLine[J];

      for I := 0 to FBGRABmp.Width - 1 do
      begin
        // pMask[i] �� 0 �� 255 ��Χ�ڵ�����ǰ͸���ȣ���Ҫ * AAlpha shr 8 �õ�ʵ��ǰ͸����
        // pBGRA^[i].rgbReserved �� 0 �� 255 ��Χ�ڵĺ�͸����
        // ����Ŀ��͸���� = (65536 - (255 - ForeAlpha)(255 - BkAlpha)) / 255
        ForeAlpha := pMask[I] * AAlpha shr 8;
        BkAlpha := pBGRA^[I].rgbReserved;
        DiffForeAlpha := ($FF - ForeAlpha) * BkAlpha;

        // Weight �õ�Ŀ��͸����
        Weight := ($FF * $FF - ($FF - ForeAlpha) * ($FF - BkAlpha)) div $FF;
        // ��ǰ����ȫ��͸��Ҳ���� ForeAlpha Ϊ 255 ʱ�����۱���͸���ȶ��٣����͸����Ϊ 255��Ҳ������ȫ��͸��
        // ��ǰ����ȫ͸��Ҳ���� ForeAlpha Ϊ 0 ʱ�����͸����Ϊ����͸���� BkAlpha

        pBGRA^[I].rgbReserved := Weight;
        if Weight <> 0 then // 0 ��ʾȫ͸�����Ͳ���Ҫ��ɫ��
        begin
          pBGRA^[I].rgbBlue := (pFor32^[I].rgbBlue * ForeAlpha +
            (pBGRA^[I].rgbBlue * DiffForeAlpha) div $FF) div Weight;
          pBGRA^[I].rgbGreen := (pFor32^[I].rgbGreen * ForeAlpha +
            (pBGRA^[I].rgbGreen * DiffForeAlpha) div $FF) div Weight;
          pBGRA^[I].rgbRed := (pFor32^[I].rgbRed * ForeAlpha +
            (pBGRA^[I].rgbRed * DiffForeAlpha) div $FF) div Weight;
        end;
      end;
    end;

    PreMultiplyBitmap(FBGRABmp); // ����Ҫ������ PreMultiply �������������

    // ע��˴� FBGRABmp ���Ѿ������ FAAFont ԭʼ�����ˣ�
    // �����ٴκ� FAAFont �����ٴ�͸���Ȼ�ϣ�����ȫ�̸��ƹ�ȥ
    Bf.BlendOp := AC_SRC_OVER;
    Bf.BlendFlags := 0;
    Bf.SourceConstantAlpha := 255;
    Bf.AlphaFormat := 0;  // �����ٴ���͸���Ȼ���ˣ����븴��

    AlphaBlend(FAAFont.Canvas.Handle, X, Y, FBGRABmp.Width, FBGRABmp.Height,
      FBGRABmp.Canvas.Handle, 0, 0, FBGRABmp.Width, FBGRABmp.Height, Bf); // ���
  end
  else
  begin
    if (ForeBmp.Width <> Mask.Width) or (ForeBmp.Height <> Mask.Height)
      or (ForeBmp.PixelFormat <> pf24bit) then
    begin                                 // ���������ͼ
      raise EInvalidForeBmp.Create(SInvalidForeground);
      Exit;
    end;

    FRGBBmp.Width := Mask.Width;
    FRGBBmp.Height := Mask.Height;
    AAlpha := Alpha * $100 div 100;       // ͸����
    FRGBBmp.Canvas.Brush.Assign(FAAFont.Canvas.Brush);
    if FRGBBmp.Canvas.Brush.Style <> bsSolid then
      Bitblt(FRGBBmp.Canvas.Handle, 0, 0, FRGBBmp.Width, FRGBBmp.Height,
        FAAFont.Canvas.Handle, X, Y, SRCCOPY) // ͸��
    else
      FillRect(FRGBBmp.Canvas.Handle, Bounds(0, 0, FRGBBmp.Width, FRGBBmp.Height), 0);

    for J := 0 to FRGBBmp.Height - 1 do
    begin
      pMask := Mask.ScanLine(J);
      pRGB := FRGBBmp.ScanLine[J];
      pFore := ForeBmp.ScanLine[J];

      for I := 0 to FRGBBmp.Width - 1 do
      begin
        Weight := pMask^[I] * AAlpha shr 8; // ���ϵ��
        if Weight = 255 then
        begin
          pRGB^[I].rgbtBlue := pFore^[I].rgbtBlue;
          pRGB^[I].rgbtGreen := pFore^[I].rgbtGreen;
          pRGB^[I].rgbtRed := pFore^[I].rgbtRed;
        end
        else if Weight <> 0 then          // �� pMask Ϊ͸���ȣ�pFore Ϊԭʼ�������л��
        begin
          Inc(pRGB^[I].rgbtBlue, Weight * (pFore^[I].rgbtBlue - pRGB^[I].rgbtBlue) shr
            8);
          Inc(pRGB^[I].rgbtGreen, Weight * (pFore^[I].rgbtGreen - pRGB^[I].rgbtGreen) shr
            8);
          Inc(pRGB^[I].rgbtRed, Weight * (pFore^[I].rgbtRed - pRGB^[I].rgbtRed) shr 8);
        end;
      end;
    end;

    Bitblt(FAAFont.Canvas.Handle, X, Y, FRGBBmp.Width, FRGBBmp.Height,
      FRGBBmp.Canvas.Handle, 0, 0, SRCCOPY); // ���
  end;
end;

// ����ǰ������ͼ
procedure TCnAABlend.SetForeBmp(const Value: TBitmap);
begin
  FForeBmp.Assign(Value);
end;

procedure TCnAABlend.SetForeBmp32(const Value: TBitmap);
begin
  FForeBmp32.Assign(Value);
end;

{ TCnAAFont }

//--------------------------------------------------------//
// ƽ��������                                             //
//--------------------------------------------------------//

// ��ʼ��
constructor TCnAAFont.Create(ACanvas: TCanvas);
begin
  FCanvas := ACanvas;
  Mask := TCnAAMask.Create(Self);
  Blend := TCnAABlend.Create(Self);
end;

// �ͷ�
destructor TCnAAFont.Destroy;
begin
  Mask.Free;
  Blend.Free;
  inherited;
end;

// ȡ��ʾ����
function TCnAAFont.GetQuality: TAAQuality;
begin
  Result := Mask.Quality;
end;

// ������ʾ����
procedure TCnAAFont.SetQuality(const Value: TAAQuality);
begin
  Mask.Quality := Value;
end;

// �ı��ߡ���
function TCnAAFont.TextExtent(const S: string): TSize;
begin
  Result := Mask.TextExtent(S);
end;

// �ı��߶�
function TCnAAFont.TextHeight(const S: string): Integer;
begin
  Result := TextExtent(S).cy;
end;

// �ı����
function TCnAAFont.TextWidth(const S: string): Integer;
begin
  Result := TextExtent(S).cx;
end;

// ƽ���ı����
procedure TCnAAFont.TextOutput(X, Y: Integer; const S: string; Alpha: TAlpha;
  Blur: TBlurStrength; DestIsAlpha: Boolean);
begin
  if (Canvas = nil) or (S = '') then
    Exit;

  Mask.DrawMask(S);                     // ���������ɰ�
  if Blur > 0 then
    Mask.Blur(Blur);                    // ģ��
  Blend.Blend(X, Y, Canvas.Font.Color, Alpha, Mask, DestIsAlpha); // ��ǰ��ɫ���
end;

{ TCnAAFontEx }

//--------------------------------------------------------//
// ��ǿƽ��������                                         //
//--------------------------------------------------------//

// ��ʼ��
constructor TCnAAFontEx.Create(ACanvas: TCanvas);
begin
  inherited Create(ACanvas);
  FEffect := TCnAAEffect.Create(nil);
end;

// �ͷ�
destructor TCnAAFontEx.Destroy;
begin
  FEffect.Free;
  inherited;
end;

// ������ʾ���
procedure TCnAAFontEx.SetEffect(const Value: TCnAAEffect);
begin
  FEffect.Assign(Value);
end;

// ������Ӱƫ��
function TCnAAFontEx.GetShadowPoint: TPoint;
begin
  if Effect.Shadow.Enabled then
  begin
    if Effect.Shadow.OffsetX > 0 then
      Result.X := Effect.Shadow.OffsetX
    else
      Result.X := 0;
    if Effect.Shadow.OffsetY > 0 then
      Result.Y := Effect.Shadow.OffsetY
    else
      Result.Y := 0;
  end
  else
  begin
    Result.X := 0;
    Result.Y := 0;
  end;
end;

// �����ı�ƫ��
function TCnAAFontEx.GetTextPoint: TPoint;
begin
  if Effect.Shadow.Enabled then
  begin
    if Effect.Shadow.OffsetX < 0 then
      Result.X := Abs(Effect.Shadow.OffsetX)
    else
      Result.X := 0;
    if Effect.Shadow.OffsetY < 0 then
      Result.Y := Abs(Effect.Shadow.OffsetY)
    else
      Result.Y := 0;
  end
  else
  begin
    Result.X := 0;
    Result.Y := 0;
  end;
end;

// �ı��ߡ���
function TCnAAFontEx.TextExtent(const S: string): TSize;
var
  LogFont: TLogFont;
  TempFont: HFONT;
  SaveFont: TFont;
begin
  if Effect.Angle <> 0 then
  begin
    GetObject(Canvas.Font.Handle, SizeOf(TLogFont), @LogFont);
    LogFont.lfEscapement := Effect.Angle * 10;
    SaveFont := TFont.Create;
    try
      SaveFont.Assign(Canvas.Font);
      TempFont := CreateFontIndirect(LogFont);
      Canvas.Font.Handle := TempFont;
      Result := inherited TextExtent(S);
      Canvas.Font.Assign(SaveFont);
      DeleteObject(TempFont);
    finally
      SaveFont.Free;
    end;
  end
  else
    Result := inherited TextExtent(S);

  if Effect.Shadow.Enabled then
  begin
    Inc(Result.cx, Abs(Effect.Shadow.OffsetX));
    Inc(Result.cy, Abs(Effect.Shadow.OffsetY));
  end;
end;

// ��������ɫǰ��
procedure TCnAAFontEx.CreateGradual;
var
  Buf, Dst: PRGBArray;
  BufLen, Len: Integer;
  SCol, ECol: TColor;
  sr, sb, sg: Byte;
  er, eb, eg: Byte;
  BufSize: Integer;
  I, J: Integer;
  Width, Height: Integer;
begin
  if (Canvas = nil) or not Effect.Gradual.Enabled then
    Exit;

  Height := Mask.Height;
  Width := Mask.Width;
  Blend.ForeBmp.Height := Height;
  Blend.ForeBmp.Width := Width;

  if Effect.Gradual.Style in [gsLeftToRight, gsRightToLeft, gsCenterToLR] then
    BufLen := Width                     //  ����������
  else
    BufLen := Height;
  if Effect.Gradual.Style in [gsCenterToLR, gsCenterToTB] then
    Len := (BufLen + 1) div 2           // ���������
  else
    Len := BufLen;
  BufSize := BufLen * 3;
  GetMem(Buf, BufSize);
  try
    // ��������ɫ��������
    if Effect.Gradual.Style in [gsLeftToRight, gsTopToBottom] then
    begin
      SCol := ColorToRGB(Effect.Gradual.StartColor);
      ECol := ColorToRGB(Effect.Gradual.EndColor);
    end
    else begin
      SCol := ColorToRGB(Effect.Gradual.EndColor);
      ECol := ColorToRGB(Effect.Gradual.StartColor);
    end;
    sr := GetRValue(SCol);              // ��ʼɫ
    sg := GetGValue(SCol);
    sb := GetBValue(SCol);
    er := GetRValue(ECol);              // ����ɫ
    eg := GetGValue(ECol);
    eb := GetBValue(ECol);
    for I := 0 to Len - 1 do
    begin
      Buf[I].rgbtRed := sr + (er - sr) * I div Len;
      Buf[I].rgbtGreen := sg + (eg - sg) * I div Len;
      Buf[I].rgbtBlue := sb + (eb - sb) * I div Len;
    end;

    if Effect.Gradual.Style in [gsCenterToLR, gsCenterToTB] then // �Գƽ���
      for I := 0 to Len - 1 do
        Buf[BufLen - 1 - I] := Buf[I];

    if Effect.Gradual.Style in [gsLeftToRight, gsRightToLeft, gsCenterToLR] then
      for I := 0 to Height - 1 do  // ˮƽ����
        Move(Buf[0], Blend.ForeBmp.ScanLine[I]^, BufSize)
    else
      for I := 0 to Height - 1 do  // ��ֱ����
      begin
        Dst := Blend.ForeBmp.ScanLine[I];
        for J := 0 to Width - 1 do
          Dst^[J] := Buf[I];
      end;
  finally
    FreeMem(Buf);
  end;
end;

// ��������ǰ��ͼ
procedure TCnAAFontEx.CreateNoiseBmp;
var
  pLine: PRGBArray;
  X, Y: Integer;
  r, g, b: Byte;
  nr, ng, nb: Integer;
  Amount: Byte;
begin
  r := GetRValue(ColorToRGB(Canvas.Font.Color));
  g := GetGValue(ColorToRGB(Canvas.Font.Color));
  b := GetBValue(ColorToRGB(Canvas.Font.Color));
  Amount := Effect.Noise;

  Blend.ForeBmp.Height := Mask.Height;
  Blend.ForeBmp.Width := Mask.Width;

  for Y := 0 to Blend.ForeBmp.Height - 1 do
  begin
    pLine := Blend.ForeBmp.ScanLine[Y];
    for X := 0 to Blend.ForeBmp.Width - 1 do
    begin
      nr := r + Random(Amount) - Amount shr 1;
      ng := g + Random(Amount) - Amount shr 1;
      nb := b + Random(Amount) - Amount shr 1;
      if nr < 0 then
        nr := 0
      else if nr > 255 then
        nr := 255;
      if ng < 0 then
        ng := 0
      else if ng > 255 then
        ng := 255;
      if nb < 0 then
        nb := 0
      else if nb > 255 then
        nb := 255;
      pLine^[X].rgbtRed := nr;
      pLine^[X].rgbtGreen := ng;
      pLine^[X].rgbtBlue := nb;
    end;
  end;
end;

// ��ǿƽ���ı����
procedure TCnAAFontEx.TextOutput(X, Y: Integer; const S: string;
  DestIsAlpha: Boolean);
var
  TextPoint, ShadowPoint: TPoint;
  OldBrushStyle: TBrushStyle;
  ShadowMask: TCnAAMask;
  LogFont: TLogFont;
  TempFont: HFONT;
  SaveFont: TFont;
begin
  if (Canvas = nil) or (S = '') then
    Exit;

  TempFont := 0;
  SaveFont := nil;
  try
    if Effect.Angle <> 0 then
    begin
      SaveFont := TFont.Create;
      SaveFont.Assign(Canvas.Font);
      GetObject(Canvas.Font.Handle, SizeOf(TLogFont), @LogFont);
      LogFont.lfEscapement := Effect.Angle * 10;
      TempFont := CreateFontIndirect(LogFont);
      Canvas.Font.Handle := TempFont;
    end;

    if Effect.Shadow.Enabled then         // ��Ӱ����
    begin
      TextPoint := GetTextPoint;
      ShadowPoint := GetShadowPoint;
      TextPoint.X := TextPoint.X + X;
      TextPoint.Y := TextPoint.Y + Y;
      ShadowPoint.X := ShadowPoint.X + X;
      ShadowPoint.Y := ShadowPoint.Y + Y;
    end
    else
    begin
      TextPoint := Point(X, Y);
    end;

    Mask.DrawMask(S);                     // ���������ɰ�
    if Effect.Outline then
      Mask.Outline;
    if Effect.Spray > 0 then
      Mask.Spray(Effect.Spray);
    if Effect.HorzMirror then
      Mask.HorzMirror;
    if Effect.VertMirror then
      Mask.VertMirror;

    OldBrushStyle := Canvas.Brush.Style;
    if Effect.Shadow.Enabled then         // ��Ӱ����
    begin
      ShadowMask := TCnAAMask.Create(Self);
      ShadowMask.Assign(Mask);            // ��Ӱ�ɰ�
      if Effect.Shadow.Blur > 0 then
        ShadowMask.Blur(Effect.Shadow.Blur); // ��Ӱģ��
      Blend.Blend(ShadowPoint.X, ShadowPoint.Y, Effect.Shadow.Color,
        Effect.Shadow.Alpha * Effect.Alpha div 100, ShadowMask, DestIsAlpha);
      // �˴��� ShadowMask �е�ģ����Ӱ���Ƶ��� Blend.AAFont.Canvas ��
      ShadowMask.Free;
      Canvas.Brush.Style := bsClear;      // ͸��
    end;

    if Effect.Blur > 0 then               // �ı�ģ��
      Mask.Blur(Effect.Blur);

    if Effect.Texture.Enabled and Assigned(Effect.Texture.Picture.Graphic) and
      not Effect.Texture.Picture.Graphic.Empty then
    begin
      CreateForeBmp;                      // ������������ͼ
      if Effect.Noise > 0 then
        AddNoise(Effect.Noise);

      if DestIsAlpha then
        Blend.SyncForeBmp32;
      Blend.BlendEx(TextPoint.X, TextPoint.Y, Effect.Alpha, Mask, DestIsAlpha);
    end
    else if Effect.Gradual.Enabled then
    begin                                 // ��������ɫǰ��ͼ
      CreateGradual;
      if Effect.Noise > 0 then
        AddNoise(Effect.Noise);

      if DestIsAlpha then
        Blend.SyncForeBmp32;
      Blend.BlendEx(TextPoint.X, TextPoint.Y, Effect.Alpha, Mask, DestIsAlpha);
    end
    else
    begin                                 // ������
      if Effect.Noise > 0 then
      begin
        CreateNoiseBmp;

        if DestIsAlpha then
          Blend.SyncForeBmp32;
        Blend.BlendEx(TextPoint.X, TextPoint.Y, Effect.Alpha, Mask, DestIsAlpha);
      end
      else
        Blend.Blend(TextPoint.X, TextPoint.Y, Canvas.Font.Color, Effect.Alpha,
          Mask, DestIsAlpha);
    end;

    if Effect.Shadow.Enabled then
      Canvas.Brush.Style := OldBrushStyle;
  finally
    if Effect.Angle <> 0 then
    begin
      Canvas.Font.Assign(SaveFont);
      SaveFont.Free;
      DeleteObject(TempFont);
    end;
  end;
end;

// ����ƽ��ͼ
procedure TCnAAFontEx.DrawTiled(Canvas: TCanvas; Rect: TRect; G: TGraphic);
var
  R, Rows, C, Cols: Integer;
begin
  if (G <> nil) and (not G.Empty) then
  begin
    Rows := ((Rect.Bottom - Rect.Top) div G.Height) + 1;
    Cols := ((Rect.Right - Rect.Left) div G.Width) + 1;
    for R := 1 to Rows do
      for C := 1 to Cols do
        Canvas.Draw(Rect.Left + (C - 1) * G.Width, Rect.Top + (R - 1) * G.Height, G);
  end;
end;

// ��������ͼ
procedure TCnAAFontEx.CreateForeBmp;
var
  Width, Height: Integer;
begin
  if (Canvas = nil) or not Effect.Texture.Enabled or
    not Assigned(Effect.Texture.Picture) then
    Exit;

  Height := Mask.Height;
  Width := Mask.Width;
  Blend.ForeBmp.Height := Height;
  Blend.ForeBmp.Width := Width;
  Blend.ForeBmp.Canvas.Brush.Color := Canvas.Font.Color;
  Blend.ForeBmp.Canvas.Brush.Style := bsSolid;
  Blend.ForeBmp.Canvas.FillRect(Rect(0, 0, Width, Height));
  case Effect.Texture.Mode of
    tmTiled:                            // ƽ��
      with Blend.ForeBmp do
        DrawTiled(Canvas, Rect(0, 0, Width, Height),
          Effect.Texture.Picture.Graphic);
    tmStretched:                        // ����
      with Blend.ForeBmp do
        StrectchDrawGraphic(Canvas, Rect(0, 0, Width, Height),
          Effect.Texture.Picture.Graphic, Canvas.Font.Color);
    tmCenter:                           // ����
      with Effect.Texture.Picture do
        Blend.ForeBmp.Canvas.Draw((Blend.ForeBmp.Width - Graphic.Width) div 2,
          (Blend.ForeBmp.Height - Graphic.Height) div 2, Graphic);
    tmNormal:                           // ��ͨ
      with Effect.Texture.Picture do
        Blend.ForeBmp.Canvas.Draw(0, 0, Graphic);
  end;
end;

// ����������
procedure TCnAAFontEx.AddNoise(Amount: Byte);
var
  pLine: PByteArray;
  X, Y: Integer;
  Val: Integer;
begin
  for Y := 0 to Blend.ForeBmp.Height - 1 do
  begin
    pLine := Blend.ForeBmp.ScanLine[Y];
    for X := 0 to Blend.ForeBmp.Width * 3 - 1 do
    begin
      Val := pLine^[X];
      Val := Val + Random(Amount) - Amount shr 1;
      if Val < 0 then
        Val := 0
      else if Val > 255 then
        Val := 255;
      pLine^[X] := Val;
    end;
  end;
end;

{ TCnEnabledClass }

//--------------------------------------------------------//
// ��Enabled���ܵĸ���֪ͨ��                              //
//--------------------------------------------------------//

//��ֵ
procedure TCnEnabledClass.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TCnEnabledClass then
  begin
    FEnabled := TCnEnabledClass(Source).Enabled;
  end;
end;

// ����֪ͨ
procedure TCnEnabledClass.Changed;
begin
  if FEnabled then                      // ���������֪ͨ
    inherited Changed;
end;

// ����
constructor TCnEnabledClass.Create(ChangedProc: TNotifyEvent);
begin
  inherited Create(ChangedProc);
  FEnabled := False;
end;

// ���ò���
procedure TCnEnabledClass.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    if Assigned(OnChanged) then
      OnChanged(Self);
  end;
end;

{ TCnAAShadow }

//--------------------------------------------------------//
// ��Ӱ������                                             //
//--------------------------------------------------------//

// ��ֵ
procedure TCnAAShadow.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TCnAAShadow then
  begin
    FBlur := TCnAAShadow(Source).Blur;
    FColor := TCnAAShadow(Source).Color;
    FOffsetX := TCnAAShadow(Source).OffsetX;
    FOffsetY := TCnAAShadow(Source).OffsetY;
  end;
end;

// ����
constructor TCnAAShadow.Create(ChangedProc: TNotifyEvent);
begin
  inherited Create(ChangedProc);
  FBlur := 80;
  FAlpha := 70;
  FColor := $00444444;
  FOffsetX := 2;
  FOffsetY := 2;
end;

// ���ò�͸����
procedure TCnAAShadow.SetAlpha(const Value: TAlpha);
begin
  if FAlpha <> Value then
  begin
    FAlpha := Value;
    Changed;
  end;
end;

// ����ģ����
procedure TCnAAShadow.SetBlur(const Value: TBlurStrength);
begin
  if FBlur <> Value then
  begin
    FBlur := Value;
    Changed;
  end;
end;

// ������Ӱɫ
procedure TCnAAShadow.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

// ����ˮƽƫ��
procedure TCnAAShadow.SetOffsetX(const Value: TOffset);
begin
  if FOffsetX <> Value then
  begin
    FOffsetX := Value;
    Changed;
  end;
end;

// ���ô�ֱƫ��
procedure TCnAAShadow.SetOffsetY(const Value: TOffset);
begin
  if FOffsetY <> Value then
  begin
    FOffsetY := Value;
    Changed;
  end;
end;

{ TCnAAGradual }

//--------------------------------------------------------//
// ����ɫ������                                           //
//--------------------------------------------------------//

// ��ֵ
procedure TCnAAGradual.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TCnAAGradual then
  begin
    FStyle := TCnAAGradual(Source).Style;
    FStartColor := TCnAAGradual(Source).StartColor;
    FEndColor := TCnAAGradual(Source).EndColor;
  end;
end;

// ����
constructor TCnAAGradual.Create(ChangedProc: TNotifyEvent);
begin
  inherited Create(ChangedProc);
  FStyle := gsLeftToRight;
  FStartColor := clWhite;
  FEndColor := clBlack;
end;

// ���ý���ɫ
procedure TCnAAGradual.SetEndColor(const Value: TColor);
begin
  if FEndColor <> Value then
  begin
    FEndColor := Value;
    Changed;
  end;
end;

// ���ÿ�ʼɫ
procedure TCnAAGradual.SetStartColor(const Value: TColor);
begin
  if FStartColor <> Value then
  begin
    FStartColor := Value;
    Changed;
  end;
end;

// ���ý��䷽ʽ
procedure TCnAAGradual.SetStyle(const Value: TGradualStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Changed;
  end;
end;

{ TCnAATexture }

//--------------------------------------------------------//
//�������������                                          //
//--------------------------------------------------------//

// ��ֵ
procedure TCnAATexture.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TCnAATexture then
  begin
    FMode := TCnAATexture(Source).Mode;
    FPicture.Assign(TCnAATexture(Source).Picture);
  end;
end;

// ����
constructor TCnAATexture.Create(ChangedProc: TNotifyEvent);
begin
  inherited Create(ChangedProc);
  FPicture := TPicture.Create;
  FPicture.OnChange := PictureChanged;
  FMode := tmTiled;
end;

// �ͷ�
destructor TCnAATexture.Destroy;
begin
  FPicture.Free;
  inherited Destroy;
end;

// ͼ�����ݸı�
procedure TCnAATexture.PictureChanged(Sender: TObject);
begin
  Changed;
end;

// ������ʾģʽ
procedure TCnAATexture.SetMode(const Value: TTextureMode);
begin
  if FMode <> Value then
  begin
    FMode := Value;
    Changed;
  end;
end;

// ����ͼ��
procedure TCnAATexture.SetPicture(const Value: TPicture);
begin
  FPicture.Assign(Value);
end;

{ TCnAAEffect }

//--------------------------------------------------------//
// ������Ч������                                         //
//--------------------------------------------------------//

// ��ֵ
procedure TCnAAEffect.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TCnAAEffect then
  begin
    FAlpha := TCnAAEffect(Source).Alpha;
    FBlur := TCnAAEffect(Source).Blur;
    FNoise := TCnAAEffect(Source).Noise;
    FOutline := TCnAAEffect(Source).Outline;
    FHorzMirror := TCnAAEffect(Source).FHorzMirror;
    FVertMirror := TCnAAEffect(Source).FVertMirror;
    FSpray := TCnAAEffect(Source).FSpray;
    FAngle := TCnAAEffect(Source).FAngle;
    FShadow.Assign(TCnAAEffect(Source).Shadow);
    FGradual.Assign(TCnAAEffect(Source).Gradual);
    FTexture.Assign(TCnAAEffect(Source).Texture);
  end;
end;

// ����
constructor TCnAAEffect.Create(ChangedProc: TNotifyEvent);
begin
  inherited Create(ChangedProc);
  FAlpha := 100;
  FBlur := 0;
  FNoise := 0;
  FSpray := 0;
  FAngle := 0;
  FOutline := False;
  FHorzMirror := False;
  FVertMirror := False;
  FShadow := TCnAAShadow.Create(OnChildChanged);
  FGradual := TCnAAGradual.Create(OnChildChanged);
  FTexture := TCnAATexture.Create(OnChildChanged);
end;

// �ͷ�
destructor TCnAAEffect.Destroy;
begin
  FShadow.Free;
  FGradual.Free;
  FTexture.Free;
  inherited;
end;

const
  vsAlpha = 'Alpha';
  vsBlur = 'Blur';
  vsNoise = 'Noise';
  vsSpray = 'Spray';
  vsAngle = 'Angle';
  vsOutline = 'Outline';
  vsHorzMirror = 'HorzMirror';
  vsVertMirror = 'VertMirror';
  vsShadow = 'Shadow';
  vsShadowAlpha = 'ShadowAlpha';
  vsShadowBlur = 'ShadowBlur';
  vsShadowColor = 'ShadowColor';
  vsShadowOffsetX = 'ShadowOffsetX';
  vsShadowOffsetY = 'ShadowOffsetY';
  vsGradual = 'Gradual';
  vsGradualStartColor = 'GradualStartColor';
  vsGradualEndColor = 'GradualEndColor';
  vsGradualStyle = 'GradualStyle';
  vsTexture = 'Texture';
  vsTextureMode = 'TextureMode';
  vsTextPicture = 'TexturePicture';

// ��INI��װ�ز���
procedure TCnAAEffect.LoadFromIni(Ini: TCustomIniFile; const Section: string);
begin
  with Ini do
  begin
    FAlpha := ReadInteger(Section, vsAlpha, FAlpha);
    FBlur := ReadInteger(Section, vsBlur, FBlur);
    FNoise := ReadInteger(Section, vsNoise, FNoise);
    FSpray := ReadInteger(Section, vsSpray, FSpray);
    FAngle := ReadInteger(Section, vsAngle, FAngle);
    FOutline := ReadBool(Section, vsOutline, FOutline);
    FHorzMirror := ReadBool(Section, vsHorzMirror, FHorzMirror);
    FVertMirror := ReadBool(Section, vsVertMirror, FVertMirror);
    FShadow.FEnabled := ReadBool(Section, vsShadow, FShadow.FEnabled);
    FShadow.FAlpha := ReadInteger(Section, vsShadowAlpha, FShadow.FAlpha);
    FShadow.FBlur := ReadInteger(Section, vsShadowBlur, FShadow.FBlur);
    FShadow.FColor := ReadInteger(Section, vsShadowColor, FShadow.FColor);
    FShadow.FOffsetX := ReadInteger(Section, vsShadowOffsetX, FShadow.FOffsetX);
    FShadow.FOffsetY := ReadInteger(Section, vsShadowOffsetY, FShadow.FOffsetY);
    FGradual.FEnabled := ReadBool(Section, vsGradual, FGradual.FEnabled);
    FGradual.FStartColor := ReadInteger(Section, vsGradualStartColor, FGradual.FStartColor);
    FGradual.FEndColor := ReadInteger(Section, vsGradualEndColor, FGradual.FEndColor);
    FGradual.FStyle := TGradualStyle(ReadInteger(Section, vsGradualStyle, Ord(FGradual.FStyle)));
    if not (FGradual.FStyle in [Low(TGradualStyle)..High(TGradualStyle)]) then
      FGradual.FStyle := Low(TGradualStyle);
    FTexture.FEnabled := ReadBool(Section, vsTexture, FTexture.FEnabled);
    FTexture.FMode := TTextureMode(ReadInteger(Section, vsTextureMode, Ord(FTexture.FMode)));
    if not (FTexture.FMode in [Low(TTextureMode)..High(TTextureMode)]) then
      FTexture.FMode := Low(TTextureMode);
  end;
end;

// �ӱ��������INI��
procedure TCnAAEffect.SaveToIni(Ini: TCustomIniFile; const Section: string);
begin
  with Ini do
  begin
    WriteInteger(Section, vsAlpha, FAlpha);
    WriteInteger(Section, vsBlur, FBlur);
    WriteInteger(Section, vsNoise, FNoise);
    WriteInteger(Section, vsSpray, FSpray);
    WriteInteger(Section, vsAngle, FAngle);
    WriteBool(Section, vsOutline, FOutline);
    WriteBool(Section, vsHorzMirror, FHorzMirror);
    WriteBool(Section, vsVertMirror, FVertMirror);
    WriteBool(Section, vsShadow, FShadow.FEnabled);
    WriteInteger(Section, vsShadowAlpha, FShadow.FAlpha);
    WriteInteger(Section, vsShadowBlur, FShadow.FBlur);
    WriteInteger(Section, vsShadowColor, FShadow.FColor);
    WriteInteger(Section, vsShadowOffsetX, FShadow.FOffsetX);
    WriteInteger(Section, vsShadowOffsetY, FShadow.FOffsetY);
    WriteBool(Section, vsGradual, FGradual.FEnabled);
    WriteInteger(Section, vsGradualStartColor, FGradual.FStartColor);
    WriteInteger(Section, vsGradualEndColor, FGradual.FEndColor);
    WriteInteger(Section, vsGradualStyle, Ord(FGradual.FStyle));
    WriteBool(Section, vsTexture, FTexture.FEnabled);
    WriteInteger(Section, vsTextureMode, Ord(FTexture.FMode));
  end;
end;

//���ò�͸����
procedure TCnAAEffect.SetAlpha(const Value: TAlpha);
begin
  if FAlpha <> Value then
  begin
    FAlpha := Value;
    Changed;
  end;
end;

// ����ģ����
procedure TCnAAEffect.SetBlur(const Value: TBlurStrength);
begin
  if FBlur <> Value then
  begin
    FBlur := Value;
    Changed;
  end;
end;

// ���������Ե
procedure TCnAAEffect.SetOutline(const Value: Boolean);
begin
  if FOutline <> Value then
  begin
    FOutline := Value;
    Changed;
  end;
end;

// ������������
procedure TCnAAEffect.SetNoise(const Value: Byte);
begin
  if FNoise <> Value then
  begin
    FNoise := Value;
    Changed;
  end;
end;

// ����ˮƽ����
procedure TCnAAEffect.SetHorzMirror(const Value: Boolean);
begin
  if FHorzMirror <> Value then
  begin
    FHorzMirror := Value;
    Changed;
  end;
end;

// ���ô�ֱ����
procedure TCnAAEffect.SetVertMirror(const Value: Boolean);
begin
  if FVertMirror <> Value then
  begin
    FVertMirror := Value;
    Changed;
  end;
end;

// �����罦
procedure TCnAAEffect.SetSpray(const Value: TSprayRange);
begin
  if FSpray <> Value then
  begin
    FSpray := Value;
    Changed;
  end;
end;

// ������ת�Ƕ�
procedure TCnAAEffect.SetAngle(const Value: TAngle);
begin
  if FAngle <> Value then
  begin
    FAngle := Value;
    Changed;
  end;
end;

// ���ý���ɫ
procedure TCnAAEffect.SetGradual(const Value: TCnAAGradual);
begin
  FGradual.Assign(Value);
  Changed;
end;

// ������Ӱ����
procedure TCnAAEffect.SetShadow(const Value: TCnAAShadow);
begin
  FShadow.Assign(Value);
  Changed;
end;

// ���������������
procedure TCnAAEffect.SetTexture(const Value: TCnAATexture);
begin
  FTexture.Assign(Value);
  Changed;
end;

{ TCnFontLabel }

//--------------------------------------------------------//
// �����ǩ��                                             //
//--------------------------------------------------------//

// ����䶯֪ͨ
procedure TCnFontLabel.Changed;
begin
  if Assigned(FontLabels) then
    FontLabels.Changed;
end;

// ��ʼ��
constructor TCnFontLabel.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FName := '';
  FFont := TFont.Create;
  FFont.OnChange := OnEffectChanged;
  FEffect := TCnAAEffect.Create(OnEffectChanged);
  FEffect.FOwner := Self;
end;

// �ͷ�
destructor TCnFontLabel.Destroy;
begin
  FFont.Free;
  FEffect.Free;
  inherited;
end;

// ����ֵ����
procedure TCnFontLabel.Assign(Source: TPersistent);
begin
  if Source is TCnFontLabel then
  begin
    FName := TCnFontLabel(Source).FName;
    FFont.Assign(TCnFontLabel(Source).FFont);
    FEffect.Assign(TCnFontLabel(Source).FEffect);
  end
  else
    inherited;
end;

// ȡ��ʾ��
function TCnFontLabel.GetDisplayName: string;
begin
  if Name <> '' then
    Result := Name
  else
    Result := inherited GetDisplayName;
end;

// ȡ�����ǩ��
function TCnFontLabel.GetFontLabels: TCnFontLabels;
begin
  if Collection is TCnFontLabels then
    Result := TCnFontLabels(Collection)
  else
    Result := nil;
end;

// ������䶯֪ͨ
procedure TCnFontLabel.OnEffectChanged(Sender: TObject);
begin
  Changed;
end;

// ��������
procedure TCnFontLabel.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  Changed;
end;

// ���ñ�ǩ��
procedure TCnFontLabel.SetName(const Value: string);
begin
  if (Value <> '') and (AnsiCompareText(Value, FName) <> 0) and
    (Collection is TCnFontLabels) and (TCnFontLabels(Collection).IndexOf(Value) >= 0) then
    raise Exception.Create(SDuplicateString); // �����ظ�
  FName := Value;
  Changed;
end;

// ����������ʾ���
procedure TCnFontLabel.SetEffect(const Value: TCnAAEffect);
begin
  FEffect.Assign(Value);
  Changed;
end;

{ TCnFontLabels }

//--------------------------------------------------------//
// �����ǩ����                                           //
//--------------------------------------------------------//

// ������ǩ
function TCnFontLabels.AddItem(AName, AFontName: string; AFontSize: Integer;
  AFontColor: TColor; AFontEffect: TFontStyles; Shadow: Boolean;
  OffsetX, OffsetY: Integer): TCnFontLabel;
begin
  if IndexOf(AName) < 0 then
  begin
    Result := TCnFontLabel(Add);
    with Result do
    begin
      FName := AName;
      FFont.Name := AFontName;
      FFont.Size := AFontSize;
      FFont.Color := AFontColor;
      FFont.Style := AFontEffect;
      FEffect.Shadow.Enabled := Shadow;
      FEffect.Shadow.OffsetX := OffsetX;
      FEffect.Shadow.OffsetY := OffsetY;
    end;
  end
  else
    Result := nil;
end;

// ���øı�֪ͨ
procedure TCnFontLabels.Changed;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

// ����
procedure TCnFontLabels.Update(Item: TCollectionItem);
begin
  inherited;
  Changed;
end;

// ��ǩ���
procedure TCnFontLabels.Check(var AText: string; AFont: TFont;
  AEffect: TCnAAEffect);
var
  StartPos, Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin                                 // ���������ǩ
    StartPos := Pos('<' + UpperCase(Items[Index].Name) + '>',
      UpperCase(AText));
    if StartPos >= 1 then
    begin                               // �л�����
      if Assigned(AFont) then
        AFont.Assign(Items[Index].Font);
      if Assigned(AEffect) then
        AEffect.Assign(Items[Index].Effect);
      system.Delete(AText, StartPos, Length(Items[Index].Name) + 2);
    end;
  end;
end;

// ��ʼ��
constructor TCnFontLabels.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TCnFontLabel);
end;

// ȡ����
function TCnFontLabels.GetItem(Index: Integer): TCnFontLabel;
begin
  Result := TCnFontLabel(inherited Items[Index]);
end;

// ȡ����
function TCnFontLabels.IndexOf(const Name: string): Integer;
begin
  for Result := 0 to Count - 1 do
    if AnsiCompareText(Items[Result].Name, Name) = 0 then Exit;
  Result := -1;
end;

// ��������
procedure TCnFontLabels.SetItem(Index: Integer; const Value: TCnFontLabel);
begin
  inherited SetItem(Index, TCollectionItem(Value));
end;

{ TCnUserLabel }

//--------------------------------------------------------//
// �û���ǩ��                                             //
//--------------------------------------------------------//

// ����ֵ����
procedure TCnUserLabel.Assign(Source: TPersistent);
begin
  if Source is TCnUserLabel then
  begin
    FName := TCnUserLabel(Source).FName;
    FText := TCnUserLabel(Source).FText;
    FOnGetText := TCnUserLabel(Source).FOnGetText;
    FStyle := TCnUserLabel(Source).FStyle;
  end
  else
    inherited;
end;

// ���֪ͨ
procedure TCnUserLabel.Changed;
begin
  if Assigned(UserLabels) then
    UserLabels.Changed;
end;

// ��ʼ��
constructor TCnUserLabel.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FName := '';
  FText := '';
  FStyle := lsCustom;
  FOnGetText := nil;
end;

// ȡ��ʾ��
function TCnUserLabel.GetDisplayName: string;
begin
  if Name <> '' then
    Result := Name
  else
    Result := inherited GetDisplayName;
end;

// ȡ�ı�
function TCnUserLabel.GetText: string;
begin
  case Style of
    lsLeftJustify, lsCenter, lsRightJustify: Result := ''; // �����ǩ
    lsRegOwner: Result := TCnUserLabels(Collection).RegOwner; // �û���
    lsRegOrganization: Result := TCnUserLabels(Collection).RegOrganization; // ��֯��
    lsAppTitle: Result := Application.Title; // Ӧ�ó������
    lsDate: Result := DateToStr(Date);  // ��ǰ����
    lsTime: Result := TimeToStr(Time);  // ��ǰʱ��
  else
    Result := FText;                    // �Զ���
  end;
  if Assigned(OnGetText) then
    OnGetText(Self, Result);            // ȡ�ı��¼�
end;

// ȡ��ǩ��
function TCnUserLabel.GetUserLabels: TCnUserLabels;
begin
  if Collection is TCnUserLabels then
    Result := TCnUserLabels(Collection)
  else
    Result := nil;
end;

// �ı��Ƿ�洢
function TCnUserLabel.IsTextStored: Boolean;
begin
  Result := FStyle = lsCustom;
end;

// ���ñ�ǩ��
procedure TCnUserLabel.SetName(const Value: string);
begin
  if (Value <> '') and (AnsiCompareText(Value, FName) <> 0) and
    (Collection is TCnUserLabels) and (TCnUserLabels(Collection).IndexOf(Value) >= 0) then
    raise Exception.Create(SDuplicateString);
  FName := Value;
  Changed;
end;

// ���ñ�ǩ���
procedure TCnUserLabel.SetStyle(const Value: TLabelStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Changed;
  end;
end;

// �����ı�
procedure TCnUserLabel.SetText(const Value: string);
begin
  if (FStyle = lsCustom) and (FText <> Value) then
  begin
    FText := Value;
    Changed;
  end;
end;

{ TCnUserLabels }

//--------------------------------------------------------//
// �û���ǩ����                                           //
//--------------------------------------------------------//

// ��ʼ��
constructor TCnUserLabels.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TCnUserLabel);
  InitRegInfo;
end;

// �����±�ǩ
function TCnUserLabels.AddItem(AName, AText: string;
  AStyle: TLabelStyle): TCnUserLabel;
begin
  if IndexOf(AName) < 0 then
  begin
    Result := TCnUserLabel(Add);
    with Result do
    begin
      FName := AName;
      FText := AText;
      FStyle := AStyle;
    end;
  end
  else
    Result := nil;
end;

// ��ǩ�䶯֪ͨ
procedure TCnUserLabels.Changed;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

// ��ǩ���
procedure TCnUserLabels.Check(var AText: string; var Align: TAlignment);
const
  csAlignArray: array[lsLeftJustify..lsRightJustify] of TAlignment =
  (taLeftJustify, taCenter, taRightJustify);
var
  StartPos, Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin                                 // �����û���ǩ
    StartPos := Pos('<' + UpperCase(Items[Index].Name) + '>',
      UpperCase(AText));
    if StartPos >= 1 then
    begin                               // ɾ����ǩ
      system.Delete(AText, StartPos, Length(Items[Index].Name) + 2);
      case Items[Index].Style of        // �����ǩ
        lsLeftJustify, lsCenter, lsRightJustify:
          begin
            Align := csAlignArray[Items[Index].Style];
          end;
      else                              // �û�Ϊ�ı�
        system.insert(Items[Index].Text, AText, StartPos);
      end;
    end;
  end;
end;

// ����
procedure TCnUserLabels.Update(Item: TCollectionItem);
begin
  inherited;
  Changed;
end;

// ��ʼ��ע�����Ϣ
procedure TCnUserLabels.InitRegInfo;
var
  V: TOSVersionInfo;
  Reg: TRegistry;
  UserName: array[0..255] of Char;
  Size: Cardinal;
  IsWin98: Boolean;
begin
  V.dwOSVersionInfoSize := SizeOf(V);
{$IFDEF FPC}
  IsWin98 := GetVersionEx(@V) and (V.dwPlatformId = VER_PLATFORM_WIN32_WINDOWS);
{$ELSE}
  IsWin98 := GetVersionEx(V) and (V.dwPlatformId = VER_PLATFORM_WIN32_WINDOWS);
{$ENDIF}

  Reg := TRegistry.Create;
  try                                   // ��ע����ж�ȡ�û�������֯��
    Reg.Rootkey := HKEY_LOCAL_MACHINE;
    if IsWin98 then
      Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\Windows\CurrentVersion')
    else
      Reg.OpenKeyReadOnly('Software\Microsoft\Windows NT\CurrentVersion');
    RegOwner := Reg.ReadString('RegisteredOwner');
    RegOrganization := Reg.ReadString('RegisteredOrganization');
    Reg.CloseKey;
    if RegOwner = '' then
    begin
      Size := 255;
      GetUserName(UserName, Size);
      RegOwner := UserName;
    end;
  finally
    Reg.Free;
  end;
end;

// ȡ����
function TCnUserLabels.GetItem(Index: Integer): TCnUserLabel;
begin
  Result := TCnUserLabel(inherited Items[Index]);
end;

// ����������
function TCnUserLabels.IndexOf(const Name: string): Integer;
begin
  for Result := 0 to Count - 1 do
    if AnsiCompareText(Items[Result].Name, Name) = 0 then Exit;
  Result := -1;
end;

// ��������
procedure TCnUserLabels.SetItem(Index: Integer; const Value: TCnUserLabel);
begin
  inherited SetItem(Index, TCollectionItem(Value));
end;

{ TCnPackParam }

//--------------------------------------------------------//
// ���������                                             //
//--------------------------------------------------------//

// ��ֵ
procedure TCnPackParam.Assign(Source: TPersistent);
begin
  if Source is TCnPackParam then
    FOwner := TCnPackParam(Source).Owner
  else
    inherited;
end;

// ����
constructor TCnPackParam.Create(AOwner: TControl);
begin
  FOwner := AOwner;
end;

{ TCnDrag }

//--------------------------------------------------------//
// �϶����������                                         //
//--------------------------------------------------------//

function TCnDrag.GetDragCursor: TCursor;
begin
  Result := TCnMyControl(FOwner).DragCursor;
end;

function TCnDrag.GetDragKind: TDragKind;
begin
  Result := TCnMyControl(FOwner).DragKind;
end;

function TCnDrag.GetDragMode: TDragMode;
begin
  Result := TCnMyControl(FOwner).DragMode;
end;

procedure TCnDrag.SetDragCursor(const Value: TCursor);
begin
  TCnMyControl(FOwner).DragCursor := Value;
end;

procedure TCnDrag.SetDragKind(const Value: TDragKind);
begin
  TCnMyControl(FOwner).DragKind := Value;
end;

procedure TCnDrag.SetDragMode(const Value: TDragMode);
begin
  TCnMyControl(FOwner).DragMode := Value;
end;

{ TCnParentEffect }

//--------------------------------------------------------//
// Parent���������                                       //
//--------------------------------------------------------//

function TCnParentEffect.GetParentBiDiMode: Boolean;
begin
  Result := TCnMyControl(FOwner).ParentBiDiMode;
end;

function TCnParentEffect.GetParentColor: Boolean;
begin
  Result := TCnMyControl(FOwner).ParentColor;
end;

function TCnParentEffect.GetParentFont: Boolean;
begin
  Result := TCnMyControl(FOwner).ParentFont;
end;

function TCnParentEffect.GetParentShowHint: Boolean;
begin
  Result := TCnMyControl(FOwner).ParentShowHint;
end;

procedure TCnParentEffect.SetParentBiDiMode(const Value: Boolean);
begin
  TCnMyControl(FOwner).ParentBiDiMode := Value;
end;

procedure TCnParentEffect.SetParentColor(const Value: Boolean);
begin
  TCnMyControl(FOwner).ParentColor := Value;
end;

procedure TCnParentEffect.SetParentFont(const Value: Boolean);
begin
  TCnMyControl(FOwner).ParentFont := Value;
end;

procedure TCnParentEffect.SetParentShowHint(const Value: Boolean);
begin
  TCnMyControl(FOwner).ParentShowHint := Value;
end;

{ TCnCustomParam }

//--------------------------------------------------------//
// �ؼ����Ʋ�����                                         //
//--------------------------------------------------------//

// ��ʼ��
constructor TCnCustomParam.Create(AOwner: TCnAAGraphicControl;
  ChangedProc: TNotifyEvent);
begin
  inherited Create(ChangedProc);
  FOwner := AOwner;
  FBackGround := TPicture.Create;
  FBackGround.OnChange := BackGroundChanged;
  FBackGroundMode := bmCenter;
  FAlignment := taLeftJustify;
  FTransparent := False;
  FLayout := tlTop;
  FWordWrap := False;
end;

// �ͷ�
destructor TCnCustomParam.Destroy;
begin
  FBackGround.Free;
  inherited;
end;

// ��ֵ
procedure TCnCustomParam.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TCnCustomParam then
  begin
    FAlignment := TCnCustomParam(Source).Alignment;
    FWordWrap := TCnCustomParam(Source).WordWrap;
    FTransparent := TCnCustomParam(Source).Transparent;
    FLayout := TCnCustomParam(Source).Layout;
    FOwner := TCnCustomParam(Source).Owner;
    FBackGround.Assign(TCnCustomParam(Source).BackGround);
    FBackGroundMode := TCnCustomParam(Source).BackGroundMode;
  end;
end;

// ����Ϊ��
function TCnCustomParam.IsBackEmpty: Boolean;
begin
  Result := not Assigned(FBackGround.Graphic) or
    FBackGround.Graphic.Empty;
end;

// ȡ����ɫ
function TCnCustomParam.GetColor: TColor;
begin
  Result := FOwner.Color;
end;

// ȡ����
function TCnCustomParam.GetFont: TFont;
begin
  Result := FOwner.Font;
end;

// ���ñ���ɫ
procedure TCnCustomParam.SetColor(const Value: TColor);
begin
  FOwner.Color := Value;
end;

// ��������
procedure TCnCustomParam.SetFont(const Value: TFont);
begin
  FOwner.Font := Value;
end;

// ����ͼ����
procedure TCnCustomParam.BackGroundChanged(Sender: TObject);
begin
  Changed;
end;

// ����������
function TCnCustomParam.GetFontEffect: TCnAAEffect;
begin
  Result := FOwner.FAAFont.Effect;
end;

// ȡ������
procedure TCnCustomParam.SetFontEffect(const Value: TCnAAEffect);
begin
  FOwner.FAAFont.Effect.Assign(Value);
  Changed;
end;

// ���ô�ֱ����
procedure TCnCustomParam.SetLayout(const Value: TTextLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Changed;
  end;
end;

// ����ȱʡ���뷽ʽ
procedure TCnCustomParam.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Changed;
  end;
end;

// �����Զ�����
procedure TCnCustomParam.SetWordWrap(const Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    Changed;
  end;
end;

// ���ñ���ͼģʽ
procedure TCnCustomParam.SetBackGroundMode(const Value: TBackGroundMode);
begin
  if FBackGroundMode <> Value then
  begin
    FBackGroundMode := Value;
    Changed;
  end;
end;

// ȡ��ʾ����
function TCnCustomParam.GetQuality: TAAQuality;
begin
  Result := FOwner.FAAFont.Quality;
end;

// ������ʾ����
procedure TCnCustomParam.SetQuality(const Value: TAAQuality);
begin
  if FOwner.FAAFont.Quality <> Value then
  begin
    FOwner.FAAFont.Quality := Value;
    Changed;
  end;
end;

// ����͸��
procedure TCnCustomParam.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Changed;
  end;
end;

// ���ñ���ͼ
procedure TCnCustomParam.SetBackGround(const Value: TPicture);
begin
  FBackGround.Assign(Value);
end;

// �洢����ɫ
function TCnCustomParam.IsColorStroed: Boolean;
begin
  Result := not FOwner.ParentColor;
end;

{ TCnCustomTextParam }

//--------------------------------------------------------//
// �ɶ��Ƶ��ı�������                                     //
//--------------------------------------------------------//

// ��ʼ��
constructor TCnCustomTextParam.Create(AOwner: TCnAAGraphicControl;
  ChangedProc: TNotifyEvent);
begin
  inherited;
  FLines := TStringList.Create;
  TStringList(FLines).OnChange := LinesChanged;
  FFontEffect := TCnAAEffect.Create(OnChildChanged);
  FRowPitch := 20;
  FLabelEffect := leUntilNextLabel;
end;

// �ͷ�
destructor TCnCustomTextParam.Destroy;
begin
  FLines.Free;
  FFontEffect.Free;
  inherited;
end;

// ��ֵ
procedure TCnCustomTextParam.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TCnCustomTextParam then
  begin
    FRowPitch := TCnCustomTextParam(Source).RowPitch;
    FLabelEffect := TCnCustomTextParam(Source).LabelEffect;
    FLines.Assign(TCnCustomTextParam(Source).Lines);
  end;
end;

// �ı������Ƿ�洢
function TCnCustomTextParam.IsLinesStored: Boolean;
begin
  Result := True;
end;

// �ı����ݸı�
procedure TCnCustomTextParam.LinesChanged(Sender: TObject);
begin
  Changed;
end;

// �����ı�����
procedure TCnCustomTextParam.SetLines(const Value: TStrings);
begin
  FLines.Assign(Value);
  Changed;
end;

// �����м��
procedure TCnCustomTextParam.SetRowPitch(const Value: TRowPitch);
begin
  if FRowPitch <> Value then
  begin
    FRowPitch := Value;
    Changed;
  end;
end;

// ���ñ�ǩ���÷�Χ
procedure TCnCustomTextParam.SetLabelEffect(const Value: TLabelEffect);
begin
  if FLabelEffect <> Value then
  begin
    FLabelEffect := Value;
    Changed;
  end;
end;

// ����Ĭ��������
procedure TCnCustomTextParam.SetFontEffect(const Value: TCnAAEffect);
begin
  FFontEffect.Assign(Value);
end;

{ TCnAAGraphicControl }

//--------------------------------------------------------//
// ƽ������ؼ�����                                       //
//--------------------------------------------------------//

// ��ʼ��
constructor TCnAAGraphicControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDrag := TCnDrag.Create(Self);
  FParentEffect := TCnParentEffect.Create(Self);
  FAAFont := TCnAAFontEx.Create(Canvas);
  FAAFont.Effect.OnChanged := OnEffectChanged;
{$IFNDEF COMPILER6_UP}
  FAutoSize := True;
{$ELSE}
  AutoSize := True;
{$ENDIF}
  FAutoUpdate := True;
  FBorder := 0;
  FUpdateCount := 0;
  Inited := False;
  Drawing := False;
  AHeight := 0;
  AWidth := 0;
end;

// �ͷ�
destructor TCnAAGraphicControl.Destroy;
begin
  FAAFont.Free;
  FDrag.Free;
  FParentEffect.Free;
  inherited;
end;

// �����ı�
procedure TCnAAGraphicControl.Changed;
begin
  if Inited and AutoUpdate and ([csLoading, csDestroying, csReading,
    csUpdating, csWriting] * ComponentState = []) and (FUpdateCount
    = 0) then
    Reset;
end;

// ��������
procedure TCnAAGraphicControl.Reset;
begin
  Invalidate;
end;

// ����ʱ������װ��
procedure TCnAAGraphicControl.Loaded;
begin
  inherited;
  Inited := True;
  LoadedEx;
end;

// ������װ�أ����ʱ������ʱ����������
procedure TCnAAGraphicControl.LoadedEx;
begin
  Changed;
end;

// �ؼ��ػ�
procedure TCnAAGraphicControl.Paint;
begin
  if [csLoading, csDestroying, csReading, csUpdating, csWriting]
    * ComponentState <> [] then
    Exit;
  if not Inited then
  begin
    Inited := True;
    LoadedEx;
  end;
  if not Visible and not (csDesigning in ComponentState) then
    Exit;
  if Drawing then
    Exit;
  Drawing := True;
  PaintCanvas;
  Drawing := False;
end;

// �ػ滭�����ӿؼ������ظ÷��������
procedure TCnAAGraphicControl.PaintCanvas;
begin

end;

// ��Ϣ����
procedure TCnAAGraphicControl.WndProc(var message: TMessage);
begin
  case message.Msg of
    CM_COLORCHANGED, CM_TEXTCHANGED, CM_FONTCHANGED:
      Changed;
    CM_MOUSEENTER:
      if Assigned(FOnMouseEnter) then
        FOnMouseEnter(Self);
    CM_MOUSELEAVE:
      if Assigned(FOnMouseLeave) then
        FOnMouseLeave(Self);
  end;
  inherited;
end;

// ��С�仯
procedure TCnAAGraphicControl.Resize;
begin
  if (Height <> AHeight) or (Width <> AWidth) then
  begin
    AHeight := Height;
    AWidth := Width;
    Changed;
  end;
  inherited Resize;
end;

// ������仯
procedure TCnAAGraphicControl.OnEffectChanged(Sender: TObject);
begin
  Changed;
end;

// �����Զ���С
{$IFDEF COMPILER6_UP}
procedure TCnAAGraphicControl.SetAutoSize(Value: Boolean);
begin
  inherited;
  Changed;
end;

{$ELSE}
procedure TCnAAGraphicControl.SetAutoSize(const Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    Changed;
  end;
end;
{$ENDIF}

// ���ñ߽���
procedure TCnAAGraphicControl.SetBorder(const Value: TBorderWidth);
begin
  if FBorder <> Value then
  begin
    FBorder := Value;
    Changed;
  end;
end;

// �����϶�����
procedure TCnAAGraphicControl.SetDrag(const Value: TCnDrag);
begin
  FDrag.Assign(Value);
end;

// ��������
procedure TCnAAGraphicControl.SetParentEffect(const Value: TCnParentEffect);
begin
  FParentEffect.Assign(Value);
end;

// �����Զ�����
procedure TCnAAGraphicControl.SetAutoUpdate(const Value: Boolean);
begin
  if FAutoUpdate <> Value then
  begin
    FAutoUpdate := Value;
    if FAutoUpdate and (csDesigning in ComponentState) then
      Changed;
  end;
end;

// ��ʼ����
procedure TCnAAGraphicControl.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

// ��������
procedure TCnAAGraphicControl.EndUpdate;
begin
  Dec(FUpdateCount);
end;

// ͼ����
procedure TCnAAGraphicControl.Blend(DesBmp, BkBmp, ForeBmp: TBitmap;
  AProgress: TProgress);
var
  pMem, pHot, pBlend: PByteArray;
  X, Y, I: Integer;
  Weight: Byte;
  AHeight, AWidth: Integer;
  Table: array[-255..255] of Byte;
begin
  AHeight := Min(BkBmp.Height, ForeBmp.Height);
  AWidth := Min(BkBmp.Width, ForeBmp.Width);
  DesBmp.Height := AHeight;
  DesBmp.Width := AWidth;
  if AProgress = 0 then
  begin
    DesBmp.Canvas.Draw(0, 0, BkBmp);
  end
  else if AProgress = csMaxProgress then
  begin
    DesBmp.Canvas.Draw(0, 0, ForeBmp);
  end
  else
  begin
    Weight := Round(AProgress * $FF / csMaxProgress);
    for I := Low(Table) to High(Table) do
      Table[I] := I * Weight shr 8;
    for Y := 0 to AHeight - 1 do
    begin
      pMem := BkBmp.ScanLine[Y];
      pHot := ForeBmp.ScanLine[Y];
      pBlend := DesBmp.ScanLine[Y];
      for X := 0 to AWidth * 3 - 1 do
        pBlend[X] := Table[pHot[X] - pMem[X]] + pMem[X];
    end;
  end;
end;

// �Ӹ��ؼ����Ʊ���
// ����������� RxLibrary VCLUtils
procedure TCnAAGraphicControl.CopyParentImage(Dest: TCanvas);
var
  I, Count, X, Y, SaveIndex: Integer;
  DC: HDC;
  R, SelfR, CtlR: TRect;
begin
  if Parent = nil then Exit;
  Count := Parent.ControlCount;
  DC := Dest.Handle;
  with Parent do
    ControlState := ControlState + [csPaintCopy];
  try
    SelfR := Bounds(Left, Top, Width, Height);
    X := -Left;
    Y := -Top;
    { Copy parent control image }
    SaveIndex := SaveDC(DC);
    try
      SetViewportOrgEx(DC, X, Y, nil);
      IntersectClipRect(DC, 0, 0, Parent.ClientWidth,
        Parent.ClientHeight);
      try
        with TCnParentControl(Parent) do
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
      if Parent.Controls[I] = Self then
        Break
      else if (Parent.Controls[I] <> nil) and
        (Parent.Controls[I] is TGraphicControl) then
      begin
        with TGraphicControl(Parent.Controls[I]) do
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
    with Parent do
      ControlState := ControlState - [csPaintCopy];
  end;
end;

// ����ƽ��ͼ
procedure TCnAAGraphicControl.DrawTiled(Canvas: TCanvas; Rect: TRect; G: TGraphic);
var
  R, Rows, C, Cols: Integer;
begin
  if (G <> nil) and (not G.Empty) then
  begin
    Rows := ((Rect.Bottom - Rect.Top) div G.Height) + 1;
    Cols := ((Rect.Right - Rect.Left) div G.Width) + 1;
    for R := 1 to Rows do
      for C := 1 to Cols do
        Canvas.Draw(Rect.Left + (C - 1) * G.Width, Rect.Top + (R - 1) * G.Height, G);
  end;
end;

// ���Ʊ���ͼ
procedure TCnAAGraphicControl.DrawBackGround(Canvas: TCanvas; Rect: TRect;
  G: TGraphic; Mode: TBackGroundMode);
var
  AStyle: TBrushStyle;
begin
  AStyle := Canvas.Brush.Style;
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(Rect);
  Canvas.Brush.Style := AStyle;
  case Mode of
    bmTiled:
      DrawTiled(Canvas, Rect, G);
    bmStretched:
      StrectchDrawGraphic(Canvas, Rect, G, Canvas.Brush.Color);
    bmCenter:
      Canvas.Draw((Rect.Right + Rect.Left - G.Width) div 2,
        (Rect.Bottom + Rect.Top - G.Height) div 2, G);
    bmNormal:
      Canvas.Draw(Rect.Left, Rect.Top, G);
  end;
end;

// �Զ�����
function TCnAAGraphicControl.GetWrapText(const Line, BreakStr: string; BreakChars:
  TSysCharSet; MaxCol: Integer): string;
const
  QuoteChars = ['''', '"'];
var
  Col, Pos: Integer;
  LinePos, LineLen: Integer;
  BreakLen, BreakPos: Integer;
  QuoteChar, CurChar: Char;
  ExistingBreak: Boolean;
  DoubleCharBreak: Boolean;
begin
  if MaxCol < 2 then MaxCol := 2;
  Col := 1;
  Pos := 1;
  LinePos := 1;
  BreakPos := 0;
  QuoteChar := ' ';
  ExistingBreak := False;
  DoubleCharBreak := False;
  LineLen := Length(Line);
  BreakLen := Length(BreakStr);
  Result := '';
  while Pos <= LineLen do
  begin
    CurChar := Line[Pos];
    if {$IFDEF UNICODE}CharInSet(CurChar, LeadBytes){$ELSE}CurChar in LeadBytes{$ENDIF} then
    begin
      if Col >= MaxCol then
      begin
        DoubleCharBreak := True;
        BreakPos := Pos - 1;
      end;
      Inc(Pos);
      Inc(Col);
    end
    else if CurChar = BreakStr[1] then
    begin
      if QuoteChar = ' ' then
      begin
        ExistingBreak := CompareText(BreakStr, Copy(Line, Pos, BreakLen)) = 0;
        if ExistingBreak then
        begin
          Inc(Pos, BreakLen - 1);
          BreakPos := Pos;
        end;
      end
    end
    else if {$IFDEF UNICODE}CharInSet(CurChar, BreakChars){$ELSE}CurChar in BreakChars{$ENDIF} then
    begin
      if QuoteChar = ' ' then
        BreakPos := Pos
    end
    else if {$IFDEF UNICODE}CharInSet(CurChar, QuoteChars){$ELSE}CurChar in QuoteChars{$ENDIF} then
      if CurChar = QuoteChar then
        QuoteChar := ' '
      else if QuoteChar = ' ' then
        QuoteChar := CurChar;
    Inc(Pos);
    Inc(Col);
    if (not ({$IFDEF UNICODE}CharInSet(QuoteChar, QuoteChars){$ELSE}QuoteChar in QuoteChars{$ENDIF}) and (ExistingBreak or
      ((Col > MaxCol) and (BreakPos > LinePos)))) or DoubleCharBreak then
    begin
      Col := Pos - BreakPos;
      Result := Result + Copy(Line, LinePos, BreakPos - LinePos + 1);
      if not ({$IFDEF UNICODE}CharInSet(CurChar, QuoteChars){$ELSE}CurChar in QuoteChars{$ENDIF}) then
        while (Pos <= LineLen) and ({$IFDEF UNICODE}CharInSet(Line[Pos], BreakChars + [#13, #10]){$ELSE}Line[Pos] in BreakChars + [#13, #10]{$ENDIF}) do
          Inc(Pos);
      if not ExistingBreak and (Pos < LineLen) then
        Result := Result + BreakStr;
      Inc(BreakPos);
      LinePos := BreakPos;
      ExistingBreak := False;
      DoubleCharBreak := False;
    end;
  end;
  Result := Result + Copy(Line, LinePos, MaxInt);
end;

// �����Զ�����
procedure TCnAAGraphicControl.WrapText(const S: string; Strs: TStrings;
  Col: Integer);
begin
  if not Assigned(Strs) then Exit;

  Strs.Clear;
  Strs.Text := GetWrapText(S, #13#10, ['.', ',', '?', '!', ' ', ';', ':',
    #9, '-'], Col);
end;

{ TCnAACustomText }

//--------------------------------------------------------//
// ƽ���ı��ؼ�����                                       //
//--------------------------------------------------------//

// ��ʼ��
constructor TCnAACustomText.Create(AOwner: TComponent);
begin
  inherited;
  FFonts := TCnFontLabels.Create(Self);
  FFonts.OnChanged := OnLabelChanged;
  FLabels := TCnUserLabels.Create(Self);
  FLabels.OnChanged := OnLabelChanged;
  FFontsInited := False;
  FLabelsInited := False;
end;

// ����Ĭ������ͱ�ǩ
procedure TCnAACustomText.CreateDefault;
begin
  BeginUpdate;
  try
    if (csDesigning in ComponentState) and UseDefaultLabels
      and (Fonts.Count = 0) and not FFontsInited then
    begin
      FFontsInited := True;
      CreateDefFonts;
    end;
    if (csDesigning in ComponentState) and UseDefaultLabels
      and (Labels.Count = 0) and not FLabelsInited then
    begin
      FLabelsInited := True;
      CreateDefLabels;
    end;
  finally
    EndUpdate;
  end;
end;

// �Ƿ񴴽�Ĭ������ͱ�ǩ
function TCnAACustomText.UseDefaultLabels: Boolean;
begin
  Result := True;
end;

// ����Ĭ������
procedure TCnAACustomText.CreateDefFonts;
begin
  with Fonts do
  begin
    AddItem('Title1', '����', 12, clBlack, [fsBold], True, 2, 2);
    AddItem('Title2', '����', 9, clBlue, [fsBold], True, 1, 1);
    AddItem('Title3', '����_GB2312', 11, clBlue, [], True, 2, 2);
    AddItem('Text1', '����_GB2312', 9, clBlack, [], False, 1, 1);
    AddItem('Text2', '����_GB2312', 8, clTeal, [], True, 1, 1);
  end;
end;

// ����Ĭ�ϱ�ǩ
procedure TCnAACustomText.CreateDefLabels;
begin
  with Labels do
  begin
    AddItem('Left', '', lsLeftJustify); // �����
    AddItem('Center', '', lsCenter);    // ���Ķ���
    AddItem('Right', '', lsRightJustify); // �Ҷ���
    AddItem('Owner', '', lsRegOwner);   // �û���
    AddItem('Organization', '', lsRegOrganization); // ��֯��
    AddItem('AppTitle', '', lsAppTitle); // Ӧ�ó������
    AddItem('Date', '', lsDate);        // ��ǰ����
    AddItem('Time', '', lsTime);        // ��ǰʱ��
  end;
end;

// ������װ��
procedure TCnAACustomText.LoadedEx;
begin
  CreateDefault;
end;

// �ͷ�
destructor TCnAACustomText.Destroy;
begin
  FFonts.Free;
  FLabels.Free;
  inherited;
end;

// ��ǩ�䶯
procedure TCnAACustomText.OnLabelChanged(Sender: TObject);
begin
  Changed;
end;

// ��������
procedure TCnAACustomText.SetFonts(const Value: TCnFontLabels);
begin
  FFonts.Assign(Value);
end;

// �����û���ǩ
procedure TCnAACustomText.SetLabels(const Value: TCnUserLabels);
begin
  FLabels.Assign(Value);
end;

initialization
  InitGrayPal;

finalization
  DeleteObject(HGrayPal);

end.



