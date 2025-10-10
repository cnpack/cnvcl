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

unit CnAACtrls;
{* |<PRE>
================================================================================
* ������ƣ�CnPack �ؼ���
* ��Ԫ���ƣ�ƽ����Ч����ؼ���Ԫ
* ��Ԫ���ߣ�CnPack ������ �ܾ��� (zjy@cnpack.org)
*           ��ֲ��e- 
*           TCnAAMarqueeText������ (fansheng_hx@yahoo.com.cn)
* ����ƽ̨��PWin2000Pro + Delphi 5.01
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7/2005 + C++Build 5/6
* ������ע���õ�Ԫʵ�������¼����ؼ���
*           ƽ����Ч�����ǩ TCnAALabel
*           ƽ����Ч�����ӱ�ǩ TCnAALinkLabel
*           ƽ����Ч�ı��ؼ� TCnAAText
*           ƽ�������ı��ؼ� TCnAAScrollText
*           ƽ����Ļ�ı��ؼ� TCnAAMarqueeText
*           ƽ����Ч�����ı��ؼ� TCnAAFadeText
* �����£�2021.07.24
*               TCnAAScrollText ֧�� Transparent ����
*           2015.06.15
*               �޸�������Զ�� BCB Unicode ����������������
*           2007.12.29
* ��ֲ���ڣ�2006.08.18
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, {$IFDEF FPC} JwaWindows, {$ENDIF} Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ShellAPI, CnAAFont, CnTimer;

const
  // ��ʱ��ʹ�ð汾��
  verCnAAFont = '';

type

{ TCnAAFontEffect }

  TCnAAFontEffect = class(TCnCustomParam)
  {* ƽ����Ч�����ǩ�ؼ�������}
  published
    property Transparent;
    {* �ؼ��Ƿ�͸��}
    property Layout;
    {* �ı���ֱ������뷽ʽ}
    property Alignment;
    {* �ı�ˮƽ���뷽ʽ}
    property Quality;
    {* ƽ��������ʾ����}
    property FontEffect;
    {* ƽ����Ч��������}
    property BackColor;
    {* �ؼ�������ɫ}
    property BackGround;
    {* �ؼ�����ͼ��}
    property BackGroundMode;
    {* �ؼ�����ͼ����ʾģʽ}
  end;

{ TAALabel }

{$IFNDEF FPC}
{$IFDEF SUPPORT_32_AND_64}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
{$ENDIF}
  TCnAALabel = class(TCnAAGraphicControl)
  {* ƽ����Ч�����ǩ�ؼ���������ʾ�����ı����ڿؼ��� Effect �����ж�����������
     ��Ч��ʾ��ص����á�
   |<BR> ע���ÿؼ���֧�ֶ����ı��������Ҫ��ʾ�����ı����� TCnAAText �����档
   |<BR> ������ڣ���ͨ��˫���ؼ�����������������Ч����}
  private
    FEffect: TCnAAFontEffect;
    FMemBmp: TBitmap;
    procedure SetEffect(const Value: TCnAAFontEffect);
  protected
    procedure PaintCanvas; override;
    procedure Reset; override;
    procedure TransparentPaint;
    procedure DrawMem;
  public
    constructor Create(AOwner: TComponent); override;
    {* �๹����}
    destructor Destroy; override;
    {* ��������}
  published
    property AutoSize;
    {* �Ƿ��Զ����ÿؼ��ߴ�}
    property Border;
    {* �ؼ��߽籣�����}
    property Caption;
    {* �ؼ�����}
    property Font;
    {* ����}
    property Width default 46;
    {* �ؼ����}
    property Height default 12;
    {* �ؼ��߶�}
    property Effect: TCnAAFontEffect read FEffect write SetEffect;
    {* ƽ����Ч��������}
  end;

{ TCnHotLink }

  TCnHotLink = class(TCnCustomParam)
  {* ƽ����Ч���峬���ӱ�ǩ�ؼ������Ӳ�����}
  private
    FFade: Boolean;
    FUnderLine: Boolean;
    FFadeDelay: Cardinal;
    FURL: string;
    FFontEffect: TCnAAEffect;
    FColor: TColor;
    FBackColor: TColor;
    procedure SetFontEffect(const Value: TCnAAEffect);
  public
    constructor Create; reintroduce;
    {* �๹����}
    destructor Destroy; override;
    {* ��������}
    procedure Assign(Source: TPersistent); override;
    {* ����ֵ����}
  published
    property Fade: Boolean read FFade write FFade default True;
    {* �Ƿ������뵭����ʾ}
    property FadeDelay: Cardinal read FFadeDelay write FFadeDelay
      default 600;
    {* ���뵭����ʾ��ʱ}
    property Color: TColor read FColor write FColor default clBlue;
    {* ����ʱ�ĸ���ʱ��������ɫ}
    property BackColor: TColor read FBackColor write FBackColor default clBtnface;
    {* ����ʱ�ı�����ɫ}
    property FontEffect: TCnAAEffect read FFontEffect write SetFontEffect;
    {* ����ʱ��������Ч����}
    property URL: string read FURL write FURL;
    {* ���������ݻ��ļ��������磺
     |<PRE>
       https://www.cnpack.org     - ��ҳ
       mailto:zjy@cnpack.org      - �ʼ���ַ
       mailto:zjy@cnpack.org?subject=��� - ���ʼ�������ʼ���ַ����
       c:\tools\anyexe.exe      - ��ִ���ļ�
       d:\aafont\readme.txt     - �ı��ļ��������ļ�
       ������Ч�ĳ����ӵ�ַ���ļ������൱�ڡ���ʼ���˵��еġ����С�����
     |</PRE>}
    property UnderLine: Boolean read FUnderLine write FUnderLine
      default False;
    {* ����ʱ�Ƿ���ʾ�»���}
    property Transparent;
    {* ����ʱ��͸������}
    property BackGround;
    {* ����ʱ�ı���ͼ��}
    property BackGroundMode;
    {* ����ʱ�ı���ͼ����ʾģʽ}
  end;

{ TCnAALinkLabel }

  TCnFadeStyle = (fsNone, fsIn, fsOut);

{$IFNDEF FPC}
{$IFDEF SUPPORT_32_AND_64}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
{$ENDIF}
  TCnAALinkLabel = class(TCnAALabel)
  {* ƽ����Ч�����ӱ�ǩ�ؼ���������ʾ�����ӣ�֧���л�ʱ�ĵ��뵭��Ч��}
  private
    FHotBmp: TBitmap;
    FBlendBmp: TBitmap;
    FFadeTimer: TTimer;
    FFadeStyle: TCnFadeStyle;
    FProgress: TProgress;
    FHotLink: TCnHotLink;
    FMouseIn: Boolean;
    FNewProg: Double;

    procedure OnFadeTimer(Sender: TObject);
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetProgress(const Value: TProgress);
    procedure SeTCnFadeStyle(const Value: TCnFadeStyle);
    procedure SeTCnHotLink(const Value: TCnHotLink);
  protected
    property Progress: TProgress read FProgress write SetProgress;
    property FadeStyle: TCnFadeStyle read FFadeStyle write SeTCnFadeStyle;
    procedure DrawHot;
    procedure PaintCanvas; override;
    procedure SetEnabled(Value: Boolean); override;
    procedure LoadedEx; override;
  public
    constructor Create(AOwner: TComponent); override;
    {* �๹����}
    destructor Destroy; override;
    {* ��������}
    procedure Click; override;
    {* ģ���û�����ÿؼ������� HotLink �� URL ����}
  published
    property HotLink: TCnHotLink read FHotLink write SeTCnHotLink;
    {* ����������}
  end;

{ TCnTextParam }

  TCnTextParam = class(TCnCustomTextParam)
  {* ƽ����Ч�ı��ؼ�������}
  protected
    function IsLinesStored: Boolean; override;
  public
    constructor Create(AOwner: TCnAAGraphicControl; ChangedProc:
      TNotifyEvent); override;
    {* �๹����}
    destructor Destroy; override;
    {* ��������}
  published
    property WordWrap;
    {* �Ƿ������Զ�����}
    property RowPitch;
    {* �ı��м�࣬��λΪ����߶ȵİٷֱ�}
    property Lines;
    {* �ı��������ԣ�����ʹ�������ǩ���û���ǩ������ÿһ���ı��Ķ��뷽ʽ��������Ч��
       ʹ�ñ�ǩʱ��һ�Լ�����'<'��'>'����ǩ�������������Ʊ�ǩ�����÷�Χ��LabelEffect
       ����������ı��ؼ��� Fonts��Labels ���ԡ�}
    property Transparent;
    {* �Ƿ�����ؼ�͸��}
    property Alignment;
    {* Ĭ�ϵ��ı����뷽ʽ������ı����ж����ǩ�����ɶ����ǩ������
     |<BR> ��� LabelEffect��Lines��Labels ����}
    property Quality;
    {* ƽ������ʾ����}
    property FontEffect;
    {* Ĭ�ϵ�������Ч����������ı����������ǩ�����������ǩ������
     |<BR> ��� LabelEffect��Lines��Fonts��Font ����}
    property LabelEffect;
    {* ���塢�����ǩ���÷�Χ}
    property BackColor;
    {* �ؼ�������ɫ}
    property BackGround;
    {* �ؼ�����ͼ��}
    property BackGroundMode;
    {* �ؼ�������ʾģʽ}
  end;

{ TCnAAText }

{$IFNDEF FPC}
{$IFDEF SUPPORT_32_AND_64}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
{$ENDIF}
  TCnAAText = class(TCnAACustomText)
  {* ƽ����Ч�ı��ؼ���������ʾ�����ı���ͨ��ʹ�ñ�ǩ������ÿ���ı�ʹ�ò�ͬ��
     ���뷽ʽ��������Ч��}
  private
    FText: TCnTextParam;
    procedure SetText(const Value: TCnTextParam);
  protected
    FTextBmp: TBitmap;
    procedure PaintCanvas; override;
    procedure LoadedEx; override;
    function UseDefaultLabels: Boolean; override;
    procedure CalcSize;
    procedure DrawCanvas(ACanvas: TCanvas);
    procedure CreateText;
    procedure TransparentPaint;
    procedure Reset; override;
  public
    constructor Create(AOwner: TComponent); override;
    {* �๹����}
    destructor Destroy; override;
    {* ��������}
  published
    property AutoSize;
    {* �Ƿ��Զ����ÿؼ��ߴ�}
    property Border;
    {* �ؼ��߽籣�����}
    property Font;
    {* �ؼ�����}
    property Width default 46;
    {* �ؼ����}
    property Height default 12;
    {* �ؼ��߶�}
    property Text: TCnTextParam read FText write SetText;
    {* �ؼ��ı����ݼ���ʾ����}
  end;

  TCnAAScrollText = class;

{ TCnScrollTextParam }

  TCnScrollTextParam = class(TCnCustomTextParam)
  {* ƽ�������ı��ؼ�������}
  private
    FFade: Boolean;
    FFadeHeight: Integer;
    FTailSpace: Integer;
    FHeadSpace: Integer;

    procedure SetFade(const Value: Boolean);
    procedure SetFadeHeight(const Value: Integer);
    procedure SetTailSpace(const Value: Integer);
    procedure SetHeadSpace(const Value: Integer);
  protected
    function IsLinesStored: Boolean; override;
  public
    constructor Create(AOwner: TCnAAGraphicControl; ChangedProc:
    {* �๹����}
      TNotifyEvent); override;
    destructor Destroy; override;
    {* ��������}
  published
    property Fade: Boolean read FFade write SetFade default True;
    {* �Ƿ�����ؼ����±߽絭�뵭��}
    property FadeHeight: Integer read FFadeHeight write SetFadeHeight default 10;
    {* ���뵭���߽�ĸ߶�}
    property HeadSpace: Integer read FHeadSpace write SetHeadSpace default 0;
    {* ��������ͷ���հ׸߶ȣ���λΪ�ؼ��߶ȵİٷֱ�}
    property TailSpace: Integer read FTailSpace write SetTailSpace default 60;
    {* ��������β���հ׸߶ȣ���λΪ�ؼ��߶ȵİٷֱ�}
    property Alignment default taCenter;
    {* Ĭ�ϵ��ı����뷽ʽ������ı����ж����ǩ�����ɶ����ǩ������
     |<BR> ��� LabelEffect��Lines��Labels ����}
    property RowPitch;
    {* �ı��м�࣬��λΪ����߶ȵİٷֱ�}
    property WordWrap;
    {* �Ƿ������Զ�����}
    property Lines;
    {* �ı��������ԣ�����ʹ�������ǩ���û���ǩ������ÿһ���ı��Ķ��뷽ʽ��������Ч��
       ʹ�ñ�ǩʱ��һ�Լ�����'<'��'>'����ǩ�������������Ʊ�ǩ�����÷�Χ��LabelEffect
       ����������ı��ؼ��� Fonts��Labels ���ԡ�}
    property Quality;
    {* ƽ������ʾ����}
    property FontEffect;
    {* Ĭ�ϵ�������Ч����������ı����������ǩ�����������ǩ������
     |<BR> ��� LabelEffect��Lines��Fonts��Font ����}
    property LabelEffect;
    {* ���塢�����ǩ���÷�Χ}
    property Font;
    {* Ĭ�ϵ��������������ı����������ǩ�����������ǩ������
     |<BR> ��� LabelEffect��Lines��Fonts ����}
    property BackColor default clWhite;
    {* �ؼ�������ɫ}
    property BackGround;
    {* �ؼ�����ͼ��}
    property BackGroundMode default bmTiled;
    {* �ؼ�������ʾģʽ}
  end;

{ TCnAAScrollText }

{$IFNDEF FPC}
{$IFDEF SUPPORT_32_AND_64}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
{$ENDIF}
  TCnAAScrollText = class(TCnAACustomText)
  {* ƽ�������ı��ؼ������ڶ����ı��Ķ�̬������ʾ}
  private
    FScrollDelay: Word;
    FScrollStep: Integer;
    FRepeatDelay: Word;
    FRepeatCount: TBorderWidth;
    FRepeatedCount: Integer;
    FText: TCnScrollTextParam;
    FCurrPos: Integer;
    FTextBmp: TBitmap;
    FCurrBmp: TBitmap;
    FDelayTimer: TTimer;
    FScrollTimer: TCnTimer;
    FActive: Boolean;
    FTransparent: Boolean;

    procedure CreateText;
    procedure OnDelayTimer(Sender: TObject);
    procedure OnScrollTimer(Sender: TObject);
    procedure SetActive(const Value: Boolean);
    procedure SetScrollDelay(const Value: Word);
    procedure SetScrollStep(const Value: Integer);
    procedure SetRepeatDelay(const Value: Word);
    procedure SetRepeatCount(const Value: TBorderWidth);
    procedure SetText(const Value: TCnScrollTextParam);
    procedure SetCurrPos(const Value: Integer);
    function GetBmpHeight: Integer;
    procedure SetTransparent(const Value: Boolean);
  protected
    procedure CreateDefFonts; override;
    procedure PaintCanvas; override;
    function UseDefaultLabels: Boolean; override;
    procedure LoadedEx; override;
{$IFNDEF FPC}
    function CanResize(var NewWidth, NewHeight: Integer): Boolean; override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    {* �๹����}
    destructor Destroy; override;
    {* ��������}
    procedure Reset; override;
    {* ���´����������ݣ�����AutoUpdateΪ��ʱ���������ڶ�̬�޸Ŀؼ��������ʼ���ؼ�������}
    procedure ReStart;
    {* ���¿�ʼ��������������������ı���ͷ��ʼ����}
    property RepeatedCount: Integer read FRepeatedCount;
    {* ��ѭ������������������ֻ������}
    property CurrPos: Integer read FCurrPos write SetCurrPos;
    {* ��ǰ��ʾ������������ͼ���е�λ�ã��û����������ֶ����ƿؼ�����}
    property BmpHeight: Integer read GetBmpHeight;
    {* ����ͼ��ĸ߶�}
  published
    property AutoUpdate;
    {* �Ƿ�����ؼ��������ʱ�Զ����´����������ݡ�����кܶ������Ҫ������ʱ���ã�
       �ɽ���������Ϊ False�����趨���������� Reset ������}
    property Active: Boolean read FActive write SetActive default True;
    {* �Ƿ������ı�����}
    property Height default 280;
    {* �ؼ��߶�}
    property Width default 240;
    {* �ؼ����}
    property ScrollDelay: Word read FScrollDelay write SetScrollDelay default 60;
    {* ����ʱ����ʱ����λΪ����}
    property ScrollStep: Integer read FScrollStep write SetScrollStep default 1;
    {* һ�ι�����������������趨Ϊ���������¹���}
    property RepeatCount: TBorderWidth read FRepeatCount write SetRepeatCount default 0;
    {* ����ѭ��������ָ��������ѭ���������Զ�ֹͣ������������ OnComplete �¼���
     |<BR> ��ֵ��Ϊ 0 ������ѭ����}
    property RepeatDelay: Word read FRepeatDelay write SetRepeatDelay default 2000;
    {* ���һ�ι���ѭ�������ʱ���������Ҫ��ʱ������Ϊ 0}
    property Text: TCnScrollTextParam read FText write SetText;
    {* �����ı����ݺͲ�������}
    property Transparent: Boolean read FTransparent write SetTransparent;
    {* �����Ƿ�͸��}
    property OnComplete;
    {* ָ�������Ĺ���ѭ�������¼����� RepeatCount}
    property OnTextReady;
    {* ���������ѳ�ʼ���¼�}
    property OnPainted;
    {* �ؼ��ػ��¼�}
  end;
  
{ TCnAAMarqueeText }

{$IFNDEF FPC}
{$IFDEF SUPPORT_32_AND_64}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
{$ENDIF}
  TCnAAMarqueeText = class(TCnAAGraphicControl)
   {* ƽ����Ļ�ı��ؼ��������ı���ˮƽ������ʾ}
  private
    {* �������� }
    FScrollType: THoriScrollType;
    FActive: Boolean;
    FSteps: Integer;
    FScrollStep: Word;
    FCurrentStep: Integer;
    FTextWidth: Integer;
    FScrollDelay: Word;
    FTimer: TTimer;
    FEffect: TCnAAFontEffect;
    FOnPainted: TNotifyEvent;
  protected
    procedure SetScrollType(Value: THoriScrollType);
    procedure SetActive(Value: Boolean );
    procedure SetScrollStep(Value: Word );
    procedure SetScrollDelay(Value: Word );
    procedure OnTimer(Sender: TObject);
    procedure SetEffect(const Value: TCnAAFontEffect);

    procedure PaintCanvas; override;
  public
    constructor Create(AOwner: TComponent); override;
    {* �๹����}
    destructor Destroy; override;
    {* ��������}
    procedure Reset; override;
    {* ���´����������ݣ����ڹ��������仯ʱ�������ù���������}
  published  
    property Active: Boolean read FActive write SetActive default False;
    {* �Ƿ������ı����뵭���л�} 
    property Height default 34;
    {* �ؼ��߶�}
    property Width default 240;
    {* �ؼ����}
    property Font;
    {* �ؼ�����}
    property Caption;
    {* �ؼ�����}
    property AutoSize;
    {* �����ô�С}
    property Effect: TCnAAFontEffect read FEffect write SetEffect;
    {* ƽ����Ч��������}
    property ScrollType: THoriScrollType read FScrollType write SetScrollType;
    {* ˮƽ��������}
    property ScrollStep: Word read FScrollStep write SetScrollStep;
    {* ˮƽ��������}
    property ScrollDelay: Word read FScrollDelay write SetScrollDelay;
    {* ����ʱ����}
    property OnPainted: TNotifyEvent read FOnPainted write FOnPainted;
    {* �ؼ��ػ��¼�}
  end;

{ TCnFadeTextParam }

  TCnFadeTextParam = class(TCnCustomTextParam)
  {* ƽ����Ч�����ı��ؼ�������}
  private
    FFadeDelay: Cardinal;
    procedure SetFadeDelay(const Value: Cardinal);
    procedure SetLineDelay(const Value: Cardinal);
    function GetLineDelay: Cardinal;
  protected
    function IsLinesStored: Boolean; override;
  public
    constructor Create(AOwner: TCnAAGraphicControl; ChangedProc:
      TNotifyEvent); override;
    {* �๹����}
    destructor Destroy; override;
    {* ��������}
    procedure Assign(Source: TPersistent); override;
    {* ����ֵ����}
  published
    property FadeDelay: Cardinal read FFadeDelay write SetFadeDelay default 600;
    {* �ı����뵭���л���ʱ}
    property LineDelay: Cardinal read GetLineDelay write SetLineDelay default 3000;
    {* ÿ���ı���ʾ��ʱ}
    property Lines;
    {* �ı��������ԣ�����ʹ�������ǩ���û���ǩ������ÿһ���ı��Ķ��뷽ʽ��������Ч��
       ʹ�ñ�ǩʱ��һ�Լ�����'<'��'>'����ǩ�������������Ʊ�ǩ�����÷�Χ��LabelEffect
       ����������ı��ؼ��� Fonts��Labels ���ԡ�}
    property Transparent;
    {* �Ƿ�����ؼ�͸��}
    property Alignment default taCenter;
    {* Ĭ�ϵ��ı����뷽ʽ������ı����ж����ǩ�����ɶ����ǩ������
     |<BR> ���LabelEffect��Lines��Labels����}
    property Layout default tlCenter;
    {* �ı���ֱ������뷽ʽ}
    property Quality;
    {* ƽ������ʾ����}
    property FontEffect;
    {* Ĭ�ϵ�������Ч����������ı����������ǩ�����������ǩ������
     |<BR> ��� LabelEffect��Lines��Fonts��Font ����}
    property LabelEffect;
    {* ���塢�����ǩ���÷�Χ}
    property BackColor default clWhite;
    {* �ؼ�������ɫ}
    property BackGround;
    {* �ؼ�����ͼ��}
    property BackGroundMode;
    {* �ؼ�������ʾģʽ}
  end;

{ TCnAAFadeText }

{$IFNDEF FPC}
{$IFDEF SUPPORT_32_AND_64}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
{$ENDIF}
  TCnAAFadeText = class(TCnAACustomText)
  {* ƽ����Ч�����ı��ؼ������ڶ����ı��ĵ��뵭���л���ʾ}
  private
    FActive: Boolean;
    FLineIndex: Integer;
    FText: TCnFadeTextParam;
    FFadeProgress: TProgress;
    FInBmp, FOutBmp, FTextBmp: TBitmap;
    FFadeTimer: TTimer;
    FDelayTimer: TTimer;
    LastText: string;
    CurrText: string;
    CurrAlign: TAlignment;
    FRepeatedCount: Integer;
    FRepeatCount: TBorderWidth;
    FNewProg: Double;

    procedure SetActive(const Value: Boolean);
    procedure SetLineIndex(const Value: Integer);
    procedure SetText(const Value: TCnFadeTextParam);
    procedure OnFadeTimer(Sender: TObject);
    procedure OnDelayTimer(Sender: TObject);
    procedure SetFadeProgress(const Value: TProgress);
    procedure DrawFadeBmp(AText: string; Bmp: TBitmap);
    procedure SetRepeatCount(const Value: TBorderWidth);
  protected
    procedure CreateDefFonts; override;
    procedure PaintCanvas; override;
    function UseDefaultLabels: Boolean; override;
    procedure LoadedEx; override;
    procedure Reset; override;
    property FadeProgress: TProgress read FFadeProgress write SetFadeProgress;
  public
    constructor Create(AOwner: TComponent); override;
    {* �๹����}
    destructor Destroy; override;
    {* ��������}
    property LineIndex: Integer read FLineIndex write SetLineIndex;
    {* ��ǰ��ʾ���������ţ��û����ֶ�����}
    property RepeatedCount: Integer read FRepeatedCount;
    {* ��ѭ������������������ֻ������}
    procedure FadeTo(Line: Integer);
    {* ���뵭���л���ָ����}
    procedure FadeToNext;
    {* ���뵭���л�����һ��}
    procedure FadeToStr(AText: string);
    {* ���뵭���л���ָ���ı�}
  published
    property Active: Boolean read FActive write SetActive default True;
    {* �Ƿ������ı����뵭���л�}
    property Height default 34;
    {* �ؼ��߶�}
    property Width default 240;
    {* �ؼ����}
    property Font;
    {* �ؼ�����}
    property RepeatCount: TBorderWidth read FRepeatCount write SetRepeatCount default 0;
    {* ����ѭ��������ָ��������ѭ���������Զ�ֹͣ������������ OnComplete �¼���
     |<BR> ��ֵ��Ϊ 0 ������ѭ����}
    property Text: TCnFadeTextParam read FText write SetText;
    {* �ؼ��ı����ݺͲ�������}
    property OnComplete;
    {* ָ�������Ĺ���ѭ�������¼����� RepeatCount}
    property OnPainted;
    {* �ؼ��ػ��¼�}
  end;

implementation

{$R-}
{$OVERFLOWCHECKS OFF}

const
  csAACopyRight =
    '<Title2>��Ȩ����'#13#10 +
    '<Text1>���ؼ�Ϊ��ѿؼ�'#13#10 +
    '����������ڹ�����ҵ�����'#13#10 +
    '����˵���μ������ļ�'#13#10 +
    '�緢�ִ�������������ϵ'#13#10#13#10 +

  '<Title2>�ؼ�����'#13#10 +
    '<Text1>���ߣ��ܾ���'#13#10 +
    'Email��zjy@cnpack.org'#13#10 +
    'Http://www.cnpack.org'#13#10 +
    'CnPack ������'#13#10;

  csAACopyRightStart =
    #13#10'<Title2>�û�����'#13#10 +
    '<Text1><Owner>'#13#10 +
    '<Organization>'#13#10#13#10 +

  '<Title2>�ؼ�����'#13#10;

  csAACopyRightEnd =
    '����ʹ�ò�ͬ��������'#13#10 +
    '�Ͷ��뷽ʽ'#13#10 +
    '֧����Ӱ������ɫ���������Ч'#13#10 +
    '�ṩ���ϵͳ������'#13#10 +
    '�����Զ������'#13#10 +
    '�����������ƽ����ʾ'#13#10#13#10 +

  '<Title2>ʹ��˵��'#13#10 +
    '<Text1>�ؼ������ԡ��������¼�'#13#10 +
    '��������ļ�'#13#10#13#10 +

  '<Title2>�ر��л'#13#10 +
    '<Text1>�����������ṩ'#13#10 +
    'ƽ��������ʾ�㷨'#13#10 +
    'liwensong@hotmail.com'#13#10 +
    'http://member.netease.com/~lws'#13#10 +
    'Passion�ְ��������ؼ�ͼ��'#13#10 +
    'master@cnpack.org'#13#10#13#10 +

  '<Title2>��ע'#13#10 +
    '<Text1>�ÿؼ�Ϊ��ѿؼ�'#13#10 +
    '�����������ؼ���������'#13#10 +
    '������߷�һ��ؿ����ʼ�'#13#10 +
    '��ʾ֧��'#13#10#13#10#13#10 +

  '<Title3>CnPack ������'#13#10 +
    '2006.08'#13#10;

  csAATextCopyRight =
    '<Title1><Center>ƽ����Ч�ı��ؼ� ' + verCnAAFont + #13#10#13#10 +
    csAACopyRight;

  csAAFadeTextCopyRight =
    '<Title1><Center>ƽ����Ч�����ı��ؼ� ' + verCnAAFont + #13#10#13#10 +
    csAACopyRight + csAACopyRightStart +
    '<Text1>������ʾ���뵭���ı�'#13#10 +
    csAACopyRightEnd;

  csAAScrollTextCopyRight =
    '<Title1>ƽ�������ı��ؼ� ' + verCnAAFont + #13#10#13#10 +
    csAACopyRight + csAACopyRightStart +
    '<Text1>������ʾ�����ı���Ϣ'#13#10 +
    csAACopyRightEnd;

{$IFNDEF COMPILER6_UP}
  AC_SRC_ALPHA = $01;
{$ENDIF}

{ TCnAALabel }

//--------------------------------------------------------//
// ƽ����Ч�����ǩ                                       //
//--------------------------------------------------------//

// ��ʼ��
constructor TCnAALabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMemBmp := TBitmap.Create;
  FMemBmp.PixelFormat := pf24bit;
  FEffect := TCnAAFontEffect.Create(Self, OnEffectChanged);
  ControlStyle := ControlStyle + [csReplicatable, csSetCaption];
  Width := 46;
  Height := 12;
end;

// �ͷ�
destructor TCnAALabel.Destroy;
begin
  FEffect.Free;
  FMemBmp.Free;
  inherited;
end;

// �ػ�
procedure TCnAALabel.Reset;
begin
  if not Effect.Transparent then
    DrawMem;
  inherited;
end;

// ���ƻ�����
procedure TCnAALabel.DrawMem;
var
  OffPoint: TPoint;
  th, tw: Integer;
begin
  AAFont.Canvas := FMemBmp.Canvas;
  FMemBmp.Canvas.Font.Assign(Font); //����
  th := AAFont.TextHeight(Caption); //�ı��߶�
  tw := AAFont.TextWidth(Caption); //�ı����
  //�Զ��趨��С
  if AutoSize and (Align in [alNone, alLeft, alRight]) then
    ClientWidth := tw + 2 * Border;
  if AutoSize and (Align in [alNone, alTop, alBottom]) then
    ClientHeight := th + 2 * Border;
  case Effect.Alignment of    //ˮƽ���뷽ʽ
    taLeftJustify: OffPoint.X := Border;
    taCenter: OffPoint.X := (ClientWidth - tw) div 2;
    taRightJustify: OffPoint.X := ClientWidth - Border - tw;
  end;
  case Effect.Layout of       //��ֱ���뷽ʽ
    tlTop: OffPoint.Y := Border;
    tlCenter: OffPoint.Y := (ClientHeight - th) div 2;
    tlBottom: OffPoint.Y := ClientHeight - Border - th;
  end;
  FMemBmp.Height := ClientHeight;
  FMemBmp.Width := ClientWidth;
  FMemBmp.Canvas.Brush.Color := Color;
  FMemBmp.Canvas.Brush.Style := bsSolid;
  if Effect.Transparent then  //͸��
  begin
    CopyParentImage(FMemBmp.Canvas); //���Ƹ��ؼ�����
  end else if not Effect.IsBackEmpty then
  begin                       //���Ʊ���ͼ
    DrawBackGround(FMemBmp.Canvas, Rect(0, 0, FMemBmp.Width, FMemBmp.Height),
      Effect.BackGround.Graphic, Effect.BackGroundMode);
  end else
  begin                       //��䱳��ɫ
    FMemBmp.Canvas.FillRect(ClientRect);
  end;
  FMemBmp.Canvas.Brush.Style := bsClear;
  AAFont.TextOutput(OffPoint.X, OffPoint.Y, Caption); //ƽ���������
end;

// ͸������
procedure TCnAALabel.TransparentPaint;
var
  OffPoint: TPoint;
  th, tw: Integer;
begin
  AAFont.Canvas := Canvas;
  Canvas.Font.Assign(Font); //����
  th := AAFont.TextHeight(Caption); //�ı��߶�
  tw := AAFont.TextWidth(Caption); //�ı����
  //�Զ��趨��С
  if AutoSize and (Align in [alNone, alLeft, alRight]) then
    ClientWidth := tw + 2 * Border;
  if AutoSize and (Align in [alNone, alTop, alBottom]) then
    ClientHeight := th + 2 * Border;
  case Effect.Alignment of    //ˮƽ���뷽ʽ
    taLeftJustify: OffPoint.X := Border;
    taCenter: OffPoint.X := (ClientWidth - tw) div 2;
    taRightJustify: OffPoint.X := ClientWidth - Border - tw;
  end;
  case Effect.Layout of       //��ֱ���뷽ʽ
    tlTop: OffPoint.Y := Border;
    tlCenter: OffPoint.Y := (ClientHeight - th) div 2;
    tlBottom: OffPoint.Y := ClientHeight - Border - th;
  end;
  Canvas.Brush.Color := Color;
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Style := bsClear;
  AAFont.TextOutput(OffPoint.X, OffPoint.Y, Caption); //ƽ���������
end;

// �ؼ��ػ�
procedure TCnAALabel.PaintCanvas;
begin
  if Effect.Transparent then
    TransparentPaint
  else
    Bitblt(Canvas.Handle, 0, 0, Width, Height, FMemBmp.Canvas.Handle, 0, 0,
      SRCCOPY);
end;

// ����������Ч
procedure TCnAALabel.SetEffect(const Value: TCnAAFontEffect);
begin
  FEffect.Assign(Value);
end;

{ TCnHotLink }

//--------------------------------------------------------//
// �����Ӳ�����                                           //
//--------------------------------------------------------//

// ���Ӳ���
procedure TCnHotLink.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TCnHotLink then
  begin
    FFade := TCnHotLink(Source).Fade;
    FUnderLine := TCnHotLink(Source).UnderLine;
    FFadeDelay := TCnHotLink(Source).FadeDelay;
    FURL := TCnHotLink(Source).URL;
    FColor := TCnHotLink(Source).Color;
    FBackColor := TCnHotLink(Source).BackColor;
    FFontEffect.Assign(TCnHotLink(Source).FontEffect);
  end;
end;

// ��ʼ��
constructor TCnHotLink.Create;
begin
  inherited Create(nil, nil);
  FFade := True;
  FUnderLine := False;
  FFadeDelay := 600;
  FURL := '';
  FColor := clBlue;
  FBackColor := clBtnface;
  FFontEffect := TCnAAEffect.Create(nil);
end;

// �ͷ�
destructor TCnHotLink.Destroy;
begin
  FFontEffect.Free;
  inherited;
end;

procedure TCnHotLink.SetFontEffect(const Value: TCnAAEffect);
begin
  FFontEffect.Assign(Value);
  Changed;
end;

{ TCnAALinkLabel }

//--------------------------------------------------------//
// ƽ����Ч�����ӱ�ǩ                                     //
//--------------------------------------------------------//

// ��ʼ��
constructor TCnAALinkLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHotLink := TCnHotLink.Create;
  FHotBmp := TBitmap.Create;
  FHotBmp.PixelFormat := pf24bit;
  FBlendBmp := TBitmap.Create;
  FBlendBmp.PixelFormat := pf24bit;
  FFadeTimer := TTimer.Create(Self);
  FFadeTimer.Interval := 55;
  FFadeTimer.OnTimer := OnFadeTimer;
  FFadeTimer.Enabled := False;
  FProgress := 0;
  FFadeStyle := fsNone;
  FNewProg := 0;
end;

// �ͷ�
destructor TCnAALinkLabel.Destroy;
begin
  FHotBmp.Free;
  FBlendBmp.Free;
  FFadeTimer.Free;
  HotLink.Free;
  inherited;
end;

// ���ƻ���
procedure TCnAALinkLabel.PaintCanvas;
begin
  if FMouseIn or (FadeStyle <> fsNone) then
    Bitblt(Canvas.Handle, 0, 0, Width, Height, FBlendBmp.Canvas.Handle, 0, 0,
      SRCCOPY)
  else
    inherited;
end;

// ���뵭��
procedure TCnAALinkLabel.OnFadeTimer(Sender: TObject);
begin
  if Abs(FNewProg - Progress) > 1 then
    FNewProg := Progress;
  case FadeStyle of
    fsIn: begin               //����
        FNewProg := FNewProg + csMaxProgress * FFadeTimer.Interval div HotLink.FadeDelay;
        if FNewProg > csMaxProgress then
        begin
          FNewProg := csMaxProgress;
          FadeStyle := fsNone;
        end;
        Progress := Round(FNewProg);
      end;
    fsOut: begin              //����
        FNewProg := FNewProg - csMaxProgress * FFadeTimer.Interval div HotLink.FadeDelay;
        if FNewProg < 0 then
        begin
          FNewProg := 0;
          FadeStyle := fsNone;
        end;
        Progress := Round(FNewProg);
      end;
    fsNone: begin             //��
        FFadeTimer.Enabled := False;
      end;
  end;
end;

// �����ȵ㻭��
procedure TCnAALinkLabel.DrawHot;
var
  OffPoint: TPoint;
  th, tw: Integer;
  AAEffect: TCnAAEffect;
begin
  BeginUpdate;
  try
    AAEffect := TCnAAEffect.Create(nil);
    AAEffect.Assign(AAFont.Effect);

    AAFont.Canvas := FHotBmp.Canvas;
    AAFont.Effect.Assign(HotLink.FontEffect);
    FHotBmp.Canvas.Font.Assign(Font); //����
    FHotBmp.Canvas.Font.Color := HotLink.Color;
    if HotLink.UnderLine then
      FHotBmp.Canvas.Font.Style := FHotBmp.Canvas.Font.Style + [fsUnderline];
    th := AAFont.TextHeight(Caption); //�ı��߶�
    tw := AAFont.TextWidth(Caption); //�ı����
    if AutoSize and (Align = alNone) then //�Զ��趨��С
    begin
      OffPoint := Point(Border, Border);
    end else begin
      case Effect.Alignment of //ˮƽ���뷽ʽ
        taLeftJustify: OffPoint.X := Border;
        taCenter: OffPoint.X := (ClientWidth - tw) div 2;
        taRightJustify: OffPoint.X := ClientWidth - Border - tw;
      end;
      case Effect.Layout of   //��ֱ���뷽ʽ
        tlTop: OffPoint.Y := Border;
        tlCenter: OffPoint.Y := (ClientHeight - th) div 2;
        tlBottom: OffPoint.Y := ClientHeight - Border - th;
      end;
    end;
    FHotBmp.Height := ClientHeight;
    FHotBmp.Width := ClientWidth;
    FHotBmp.Canvas.Brush.Color := HotLink.BackColor;
    FHotBmp.Canvas.Brush.Style := bsSolid;
    if HotLink.Transparent then
    begin
      CopyParentImage(FHotBmp.Canvas);
    end else if not HotLink.IsBackEmpty then
    begin
      DrawBackGround(FHotBmp.Canvas, Rect(0, 0, FHotBmp.Width, FHotBmp.Height),
        HotLink.BackGround.Graphic, HotLink.BackGroundMode);
    end else
    begin
      FHotBmp.Canvas.FillRect(ClientRect);
    end;
    FHotBmp.Canvas.Brush.Style := bsClear;
    AAFont.TextOutput(OffPoint.X, OffPoint.Y, Caption); //ƽ���������

    AAFont.Effect.Assign(AAEffect);
    AAEffect.Free;
  finally
    EndUpdate;
  end;
end;

// ������뿪ʼ����
procedure TCnAALinkLabel.CMMouseEnter(var Message: TMessage);
begin
  if Enabled then
  begin
    FMouseIn := True;
    DrawMem;
    DrawHot;
    if HotLink.Fade then
    begin
      FadeStyle := fsIn;
    end else
      Progress := csMaxProgress;
  end;
  inherited;
end;

// ���Ƴ���ʼ����
procedure TCnAALinkLabel.CMMouseLeave(var Message: TMessage);
begin
  if Enabled then
  begin
    if HotLink.Fade then
    begin
      FadeStyle := fsOut;
    end else
      Progress := 0;
    FMouseIn := False;
  end;
  inherited;
end;

// ����ؼ�
procedure TCnAALinkLabel.Click;
var
  Wnd: THandle;
begin
  if HotLink.URL <> EmptyStr then
  begin
    if Parent is TForm then
      Wnd := Parent.Handle
    else
      Wnd := 0;               //NULL;
    ShellExecute(Wnd, nil, PChar(HotLink.URL), nil, nil, SW_SHOWNORMAL);
  end;
  inherited;
end;

// ������װ��
procedure TCnAALinkLabel.LoadedEx;
begin
  inherited;
  Reset;
end;

// ���õ��뵭������
procedure TCnAALinkLabel.SetProgress(const Value: TProgress);
begin
  if FProgress <> Value then
  begin
    FProgress := Value;
    Blend(FBlendBmp, FMemBmp, FHotBmp, Progress);
    Paint;
  end;
end;

// ��������
procedure TCnAALinkLabel.SetEnabled(Value: Boolean);
begin
  inherited;
  if not Value then
  begin
    FadeStyle := fsNone;
    Progress := 0;
  end;
end;

// ���õ��뵭��
procedure TCnAALinkLabel.SeTCnFadeStyle(const Value: TCnFadeStyle);
begin
  if FFadeStyle <> Value then
  begin
    FFadeStyle := Value;
    FFadeTimer.Enabled := FFadeStyle <> fsNone;
  end;
end;

// �������Ӳ���
procedure TCnAALinkLabel.SeTCnHotLink(const Value: TCnHotLink);
begin
  FHotLink.Assign(Value);
end;

{ TCnAAText }

//--------------------------------------------------------//
// ƽ����Ч�����ӱ�ǩ                                     //
//--------------------------------------------------------//

// �����ߴ�
procedure TCnAAText.CalcSize;
var
  I, J: Integer;
  DispLines: TStrings;
  WrapLines: TStrings;
  CurrText: string;
  CurrAlign: TAlignment;
  TextWidth: Integer;
  TextHeight: Integer;
  AWidth, AHeight: Integer;
  xFree, yFree: Boolean;
  MaxCol: Integer;
begin
  BeginUpdate;
  DispLines := nil;
  WrapLines := nil;
  try
    DispLines := TStringList.Create; //��ʱ�ı�
    WrapLines := TStringList.Create;
    with FText do
    begin
      xFree := not WordWrap and AutoSize and (Align in [alNone, alLeft, alRight]);
      yFree := AutoSize and (Align in [alNone, alTop, alBottom]);
      if xFree then AWidth := 0
      else AWidth := ClientWidth;
      if yFree then AHeight := 0
      else AHeight := ClientHeight;
      if xFree or yFree then
      begin
        DispLines.Clear;
        DispLines.AddStrings(Lines);
        AAFont.Canvas := Canvas;
        AAFont.Effect.Assign(FText.FontEffect);
        Canvas.Font.Assign(Font);
        for I := 0 to DispLines.Count - 1 do
        begin
          CurrText := DispLines[I]; // ��ǰ�����ַ���
          if LabelEffect = leOnlyALine then
          begin
            Canvas.Font.Assign(Font);
            AAFont.Effect.Assign(FText.FontEffect);
          end;
          Fonts.Check(CurrText, Canvas.Font, AAFont.Effect); // ��������ǩ
          Labels.Check(CurrText, CurrAlign); // ����û���ǩ
          TextWidth := AAFont.TextWidth(CurrText);
          if WordWrap and (TextWidth > AWidth) then // �Զ�����
          begin
            MaxCol := AWidth * Length(CurrText) div TextWidth;
            while AAFont.TextWidth(Copy(CurrText, 1, MaxCol)) > AWidth do
              Dec(MaxCol);
            WrapText(CurrText, WrapLines, MaxCol);
          end else if CurrText <> '' then
            WrapLines.Text := CurrText
          else
            WrapLines.Text := ' ';
          if xFree and (TextWidth > AWidth) then // ȷ�����
          begin
            AWidth := TextWidth;
          end;
          if yFree then       // ȷ���߶�
          begin
            for J := 0 to WrapLines.Count - 1 do
            begin
              CurrText := WrapLines[J];
              TextHeight := AAFont.TextHeight(CurrText + ' ');
              Inc(AHeight, TextHeight);
              if (I < DispLines.Count - 1) or (J < WrapLines.Count - 1) then
                Inc(AHeight, Round(TextHeight * RowPitch / 100));
            end;
          end;
        end;
        if xFree then ClientWidth := AWidth + 2 * Border;
        if yFree then ClientHeight := AHeight + 2 * Border;
      end;
    end;
  finally
    DispLines.Free;
    WrapLines.Free;
    EndUpdate;
  end;
end;

// ����
constructor TCnAAText.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csReplicatable];
  FText := TCnTextParam.Create(Self, OnLabelChanged);
  FTextBmp := TBitmap.Create;
  FTextBmp.PixelFormat := pf24bit;
  Width := 46;
  Height := 12;
end;

// ������ʾ�ı�
procedure TCnAAText.CreateText;
begin
  CalcSize;
  FTextBmp.Canvas.Brush.Color := Color;
  FTextBmp.Canvas.Brush.Style := bsSolid;
  FTextBmp.Width := ClientWidth;
  FTextBmp.Height := ClientHeight;
  if FText.Transparent then     // ͸��
  begin
    CopyParentImage(FTextBmp.Canvas); // ���Ƹ��ؼ�����
  end else if not FText.IsBackEmpty then
  begin                   // ���Ʊ���ͼ
    DrawBackGround(FTextBmp.Canvas, Rect(0, 0, FTextBmp.Width, FTextBmp.Height),
      FText.BackGround.Graphic, FText.BackGroundMode);
  end else
  begin                   // ��䱳��ɫ
    FTextBmp.Canvas.FillRect(ClientRect);
  end;
  FTextBmp.Canvas.Brush.Style := bsClear;
  DrawCanvas(FTextBmp.Canvas);
end;

// �ͷ�
destructor TCnAAText.Destroy;
begin
  FTextBmp.Free;
  FText.Free;
  inherited;
end;

// ����
procedure TCnAAText.DrawCanvas(ACanvas: TCanvas);
var
  I, J: Integer;
  DispLines: TStrings;
  WrapLines: TStrings;
  CurrText: string;
  CurrAlign: TAlignment;
  X, Y: Integer;
  TextWidth: Integer;
  TextHeight: Integer;
  MaxCol: Integer;
begin
  BeginUpdate;
  DispLines := nil;
  WrapLines := nil;
  try
    DispLines := TStringList.Create; // ��ʱ�ı�
    WrapLines := TStringList.Create;
    with FText do
    begin
      DispLines.AddStrings(Lines);
      ACanvas.Brush.Color := Color;
      ACanvas.Brush.Style := bsClear;
      ACanvas.Font.Assign(Font);
      AAFont.Canvas := ACanvas;
      AAFont.Effect.Assign(FText.FontEffect);
      CurrAlign := Alignment; // Ĭ�϶��뷽ʽ
      Y := Border;
      for I := 0 to DispLines.Count - 1 do
      begin
        if Y > ClientHeight - Border then
          Break;
        CurrText := DispLines[I]; // ��ǰ�����ַ���
        if LabelEffect = leOnlyALine then
        begin
          ACanvas.Font.Assign(Font);
          AAFont.Effect.Assign(FText.FontEffect);
          CurrAlign := Alignment;
        end;
        Fonts.Check(CurrText, ACanvas.Font, AAFont.Effect); // ��������ǩ
        Labels.Check(CurrText, CurrAlign); // ����û���ǩ
        TextWidth := AAFont.TextWidth(CurrText);
        if WordWrap and (TextWidth > ClientWidth - 2 * Border) then // �Զ�����
        begin
          MaxCol := (ClientWidth - 2 * Border) * Length(CurrText) div TextWidth;
          while AAFont.TextWidth(Copy(CurrText, 1, MaxCol)) > ClientWidth - 2
            * Border do
            Dec(MaxCol);
          WrapText(CurrText, WrapLines, MaxCol);
        end else if CurrText <> '' then
          WrapLines.Text := CurrText
        else
          WrapLines.Text := ' ';
        for J := 0 to WrapLines.Count - 1 do
        begin
          CurrText := WrapLines[J];
          TextHeight := AAFont.TextHeight(CurrText + ' ');
          TextWidth := AAFont.TextWidth(CurrText);
          case CurrAlign of   // ���뷽ʽ
            taLeftJustify: X := Border;
            taCenter: X := (ClientWidth - TextWidth) div 2;
            taRightJustify: X := ClientWidth - Border - TextWidth;
          else X := 0;
          end;
          AAFont.TextOutput(X, Y, CurrText);
          Y := Y + Round(TextHeight * (1 + RowPitch / 100));
        end;
      end;
      AAFont.Effect.Assign(FText.FontEffect);
    end;
  finally
    DispLines.Free;
    WrapLines.Free;
    EndUpdate;
  end;
end;

// �ؼ�������װ��
procedure TCnAAText.LoadedEx;
begin
  inherited;
  Reset;
end;

// ���ƻ���
procedure TCnAAText.PaintCanvas;
begin
  if Text.Transparent then
    TransparentPaint    // ͸��
  else
    Bitblt(Canvas.Handle, 0, 0, Width, Height, FTextBmp.Canvas.Handle, 0, 0,
      SRCCOPY);
end;

// ��λ
procedure TCnAAText.Reset;
begin
  if not Text.Transparent then
    CreateText;
  inherited;
end;

// �����ı�
procedure TCnAAText.SetText(const Value: TCnTextParam);
begin
  Text.Assign(Value);
end;

// ͸������
procedure TCnAAText.TransparentPaint;
begin
  CalcSize;
  DrawCanvas(Canvas);
end;

// Ĭ���ı�����Ĭ�ϱ�ǩ
function TCnAAText.UseDefaultLabels: Boolean;
begin
  Result := not FText.IsLinesStored;
end;

{ TCnTextParam }

//--------------------------------------------------------//
// ƽ���ı�������                                         //
//--------------------------------------------------------//

// ����
constructor TCnTextParam.Create(AOwner: TCnAAGraphicControl;
  ChangedProc: TNotifyEvent);
begin
  inherited;
  Lines.Text := csAATextCopyRight;
end;

// �ͷ�
destructor TCnTextParam.Destroy;
begin
  inherited;
end;

// �ı��洢
function TCnTextParam.IsLinesStored: Boolean;
begin
  Result := Lines.Text <> csAATextCopyRight;
end;

{ TCnAAScrollText }

//--------------------------------------------------------//
// ƽ�������ı��ؼ�                                       //
//--------------------------------------------------------//

// �ؼ���ʼ��
constructor TCnAAScrollText.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOpaque]; // �ɿؼ��������пͻ���
  FText := TCnScrollTextParam.Create(Self, OnLabelChanged);
  FTextBmp := TBitmap.Create;
  FTextBmp.PixelFormat := pf32bit;
{$IFDEF TGRAPHIC_SUPPORT_PARTIALTRANSPARENCY}
  FTextBmp.AlphaFormat := afDefined;
{$ENDIF}
  FCurrBmp := TBitmap.Create;
  FCurrBmp.PixelFormat := pf24bit;
  FScrollTimer := TCnTimer.Create(Self);
  FScrollTimer.Enabled := False;
  FScrollTimer.OnTimer := OnScrollTimer;
  FDelayTimer := TTimer.Create(Self);
  FDelayTimer.Enabled := False;
  FDelayTimer.OnTimer := OnDelayTimer;
  FCurrPos := 0;
  FRepeatCount := 0;
  FActive := True;
  RepeatDelay := 2000;
  ScrollStep := 1;
  ScrollDelay := 60;
  Color := clWhite;
  SetBounds(0, 0, 240, 280);
end;

// �ͷ�
destructor TCnAAScrollText.Destroy;
begin
  Active := False;
  FScrollTimer.Free;
  FDelayTimer.Free;
  FTextBmp.Free;
  FCurrBmp.Free;
  FText.Free;
  inherited;
end;

// ��ʾ�ı���λ
procedure TCnAAScrollText.Reset;
var
  tActive: Boolean;
begin
  tActive := Active;
  FRepeatedCount := -1;
  Active := False;
  CreateText;
  FCurrPos := 0;
  Paint;
  Active := tActive;
end;

// ���ƿؼ�
procedure TCnAAScrollText.PaintCanvas;
var
  I, FH, U, D: Integer;
  BkRed, BkGreen, BkBlue: Byte;
  tBkColor: TColor;
  Bf: TBlendFunction;

  // ͸����� TextBmp ��ָ��͸���ȵ� CurrBmp ��һ�����ߣ�Transparency �� 0 �� 255
  procedure BlendFadeOnLine(YDst, YSrc: Integer; Transparency: Integer);
  var
    SrcRow: PBGRAArray;
    DstRow: PRGBArray;
    X, NT: Integer;
    B: Byte;
  begin
    if (FCurrBmp = nil) or (FTextBmp = nil) then
      Exit;
    if (YSrc < 0) or (YSrc >= FTextBmp.Height) then
      Exit;
    if (YDst < 0) or (YDst >= FCurrBmp.Height) then
      Exit;

    SrcRow := FTextBmp.ScanLine[YSrc];
    DstRow := FCurrBmp.ScanLine[YDst];

    // ע�� TextBmp ���� PreMultiply �������ݣ�Ҳ����������͸�����Ѿ��������ˡ�
    for X := 0 to FCurrBmp.Width - 1 do
    begin
      if (Transparency > 0) and (SrcRow^[X].rgbReserved > 0) then // �в�͸���Ż��
      begin
        // Ŀ��͸����=1-��1-ǰ͸���ȣ�*��1-��͸���ȣ�
        // Ŀ�����=��ǰ����*ǰ͸���� + �����*��͸����*��1-ǰ͸���ȣ���/Ŀ��͸����
        // �˴���͸����Ϊ 1����˼򻯳ɣ�Ŀ��͸���� 1
        // Ŀ�����=��ǰ����*ǰ͸���� + �����*��1-ǰ͸���ȣ�
        NT := MulDiv(Transparency, SrcRow^[X].rgbReserved, 255); // �µ�ǰ͸����

        B := MulDiv(SrcRow^[X].rgbRed, 255, SrcRow^[X].rgbReserved);    // ��ǰ����
        DstRow^[X].rgbtRed := (NT * B + (255 - NT) * DstRow^[X].rgbtRed) div $FF;
        B := MulDiv(SrcRow^[X].rgbGreen, 255, SrcRow^[X].rgbReserved);  // ��ǰ����
        DstRow^[X].rgbtGreen := (NT * B + (255 - NT) * DstRow^[X].rgbtGreen) div $FF;
        B := MulDiv(SrcRow^[X].rgbBlue, 255, SrcRow^[X].rgbReserved);   // ��ǰ����
        DstRow^[X].rgbtBlue := (NT * B + (255 - NT) * DstRow^[X].rgbtBlue) div $FF;
      end;
    end;
  end;

  // ͸�����ǰ��ɫ��ָ��͸���ȵ� CurrBmp ��һ�����ߣ�Transparency �� 0 �� 255
  procedure DrawColorFadeOnLine(Y: Integer; Transparency: Integer);
  var
    Row: PRGBArray;
    X: Integer;
  begin
    Row := FCurrBmp.ScanLine[Y];
    for X := 0 to FCurrBmp.Width - 1 do
    begin
      if Row[X].rgbtRed <> BkRed then
        Row[X].rgbtRed := Transparency * (Row[X].rgbtRed - BkRed) shr 8 + BkRed;
      if Row[X].rgbtGreen <> BkGreen then
        Row[X].rgbtGreen := Transparency * (Row[X].rgbtGreen - BkGreen) shr 8 + BkGreen;
      if Row[X].rgbtBlue <> BkBlue then
        Row[X].rgbtBlue := Transparency * (Row[X].rgbtBlue - BkBlue) shr 8 + BkBlue;
    end;
  end;

begin
  // �����ݹ̶��İ������ֵ� TextBmp����������������зֻ��� CurrBmp ��
  // ��֧��͸��ʱ��32 λ TextBmp ͨ�� BitBlt ���Ƶ� 24 λ CurrBmp ���ٻ��Ƶ�������
  // ֧��͸��ʱ��24 λ�� CurrBmp �ȸ��Ʊ���������32 λ�� Alpha �� TextBmp ͨ�� AlphaBlend ���Ƶ� CurrBmp ��
  FCurrBmp.Height := Height;
  FCurrBmp.Width := Width;

  FH := 0;
  U := 0;
  D := 0;

  if FTransparent then
  begin
    CopyParentImage(FCurrBmp.Canvas);

    Bf.BlendOp := AC_SRC_OVER;
    Bf.BlendFlags := 0;
    Bf.SourceConstantAlpha := $FF;
    Bf.AlphaFormat := AC_SRC_ALPHA;

    if FText.Fade then
      FH := FText.FadeHeight;
  end;

  // FCurrPos ָ CurrBmp ��ԵӦ���� TextBmp ��ֱ�����Ͼ� TextBmp ��Ե�ľ���
  // U D Ϊ���� Fade ʱ TextBmp ��ֱ����Ļ�����ʼ��� TextBmp ��Ե�ľ���

  if FCurrPos + Height <= FTextBmp.Height then // TextBmp �㹻��������ʾ
  begin
    if FTransparent then  // ͸��ʱ���������� FH �ĸ߶ȹ�������
    begin
      AlphaBlend(FCurrBmp.Canvas.Handle, 0, FH, Width, Height - FH * 2,
        FTextBmp.Canvas.Handle, 0, FCurrPos + FH, Width, Height - FH * 2, Bf);

      U := FCurrPos;
      D := FCurrPos + Height;
    end
    else
      BitBlt(FCurrBmp.Canvas.Handle, 0, 0, Width, Height, FTextBmp.Canvas.Handle, 0,
        FCurrPos, SRCCopy);
  end
  else // TextBmp �����ˣ����������������β��ӻ��ƣ�͸��ʱ���������� FH �ĸ߶ȹ�������
  begin                       
    if FTransparent then
    begin
      // ������
      AlphaBlend(FCurrBmp.Canvas.Handle, 0, FH, Width, FTextBmp.Height - FCurrPos - FH,
        FTextBmp.Canvas.Handle, 0, FCurrPos + FH, Width, FTextBmp.Height - FCurrPos - FH, Bf);
      // ������
      AlphaBlend(FCurrBmp.Canvas.Handle, 0, FTextBmp.Height - FCurrPos, Width, Height -
        (FTextBmp.Height - FCurrPos) - FH, FTextBmp.Canvas.Handle, 0, 0, Width, Height -
        (FTextBmp.Height - FCurrPos) - FH, Bf);

      U := FCurrPos;
      D := Height - (FTextBmp.Height - FCurrPos);
    end
    else
    begin
      BitBlt(FCurrBmp.Canvas.Handle, 0, 0, Width, FTextBmp.Height - FCurrPos,
        FTextBmp.Canvas.Handle, 0, FCurrPos, SRCCopy);
      BitBlt(FCurrBmp.Canvas.Handle, 0, FTextBmp.Height - FCurrPos, Width, Height -
        (FTextBmp.Height - FCurrPos), FTextBmp.Canvas.Handle, 0, 0, SRCCopy);
    end;
  end;

  // ���뵭�����±�Ե
  if FText.Fade then
  begin
    if FTransparent then
    begin
      for I := 0 to FText.FadeHeight - 1 do
      begin
        BlendFadeOnLine(I, U + I, 255 * I div (FH - 1));
        BlendFadeOnLine(Height - 1 - I, D - 1 - I, 255 * I div (FH - 1));
      end;
    end
    else
    begin
      tBkColor := ColorToRGB(Color);
      BkRed := GetRValue(tBkColor);
      BkGreen := GetGValue(tBkColor);
      BkBlue := GetBValue(tBkColor);

      for I := 0 to FText.FadeHeight - 1 do
      begin
        DrawColorFadeOnLine(I, 255 * I div (FText.FadeHeight - 1));
        DrawColorFadeOnLine(Height - 1 - I, 255 * I div (FText.FadeHeight - 1));
      end;
    end;
  end;

  // ���Ƶ��ؼ�����
  if not (csDestroying in ComponentState) then
    BitBlt(Canvas.Handle, 0, 0, Width, Height, FCurrBmp.Canvas.Handle, 0, 0, SRCCopy);

  if Assigned(OnPainted) then
    OnPainted(Self);
end;

// ִ�й���
procedure TCnAAScrollText.OnScrollTimer(Sender: TObject);
begin
  if CurrPos = 0 then         // ���ι������
  begin
    FRepeatedCount := FRepeatedCount + 1;
    if (RepeatCount > 0) and (RepeatedCount >= RepeatCount) then
    begin                     // �������
      Active := False;
      FRepeatedCount := -1;
      if Assigned(OnComplete) then
        OnComplete(Self);
      Exit;
    end
    else if FDelayTimer.Interval > 0 then
    begin                     // ѭ����ʱ
      FScrollTimer.Enabled := False;
      FDelayTimer.Enabled := True;
      Exit;
    end;
  end;

  if (FScrollStep > 0) and (CurrPos + FScrollStep >= FTextBmp.Height) then
    CurrPos := 0
  else if (FScrollStep < 0) and (CurrPos + FScrollStep < 0) then
    CurrPos := 0
  else
    CurrPos := CurrPos + FScrollStep; // ��ǰλ������
end;

// �����ı�λͼ
procedure TCnAAScrollText.CreateText;
var
  I, J: Integer;
  DispLines: TStrings;
  CurrText: string;
  WrapLines: TStrings;
  CurrHeight: Integer;
  CurrAlign: TAlignment;
  X, Y: Integer;
  TextWidth: Integer;
  TextHeight: Integer;
  MaxCol: Integer;
begin
  BeginUpdate;
  DispLines := nil;
  WrapLines := nil;
  try
    DispLines := TStringList.Create; // ��ʱ�ı�
    WrapLines := TStringList.Create;
    with FText do
    begin
      FTextBmp.Height := 0;
      FTextBmp.Width := Width;
      FTextBmp.Canvas.Brush.Color := Color;
      FTextBmp.Canvas.Brush.Style := bsSolid;
      DispLines.Clear;
      DispLines.AddStrings(Lines);
      AAFont.Canvas := FTextBmp.Canvas;
      AAFont.Effect.Assign(FText.FontEffect);
      if Fade then            // ���뵭���հ�
        CurrHeight := FadeHeight
      else
        CurrHeight := 0;
      CurrHeight := CurrHeight + Height * HeadSpace div 100; // ͷ���հ�
      FTextBmp.Canvas.Font.Assign(Font);
      for I := 0 to DispLines.Count - 1 do
      begin
        CurrText := DispLines[I]; // ��ǰ�����ַ���
        if LabelEffect = leOnlyALine then
        begin
          FTextBmp.Canvas.Font.Assign(Font);
          AAFont.Effect.Assign(FText.FontEffect);
        end;
        Fonts.Check(CurrText, FTextBmp.Canvas.Font, AAFont.Effect); // ��������ǩ
        Labels.Check(CurrText, CurrAlign); // ����û���ǩ
        TextHeight := AAFont.TextHeight(CurrText + ' ');
        TextWidth := AAFont.TextWidth(CurrText);
        if WordWrap and (TextWidth > Width) then // �Զ�����
        begin
          MaxCol := Width * Length(CurrText) div TextWidth;
          while AAFont.TextWidth(Copy(CurrText, 1, MaxCol)) > Width do
            Dec(MaxCol);
          WrapText(CurrText, WrapLines, MaxCol);
        end
        else if CurrText <> '' then
          WrapLines.Text := CurrText
        else
          WrapLines.Text := ' ';
        CurrHeight := CurrHeight + Round(TextHeight * (1 + RowPitch / 100)) *
          WrapLines.Count;
      end;
      FTextBmp.Canvas.Brush.Color := Color;
      FTextBmp.Canvas.Brush.Style := bsSolid;
      CurrHeight := CurrHeight + Height * TailSpace div 100; // β���հ�
      if CurrHeight < ClientHeight then
        CurrHeight := ClientHeight;
      FTextBmp.Height := CurrHeight;

      if FTransparent then
      begin
        // ��͸���Ļ������ȫ 0
        X := FTextBmp.Width * SizeOf(TRGBQuad);
        for Y := 0 to FTextBmp.Height - 1 do
          FillChar(FTextBmp.ScanLine[Y]^, X, 0); // �����ȫ͸�������ﻭ����û��
      end;

      if Assigned(FText.BackGround.Graphic) and not
        FText.BackGround.Graphic.Empty then
        DrawBackGround(FTextBmp.Canvas, Rect(0, 0, FTextBmp.Width,
          FTextBmp.Height), FText.BackGround.Graphic, FText.BackGroundMode);

      DispLines.Clear;
      DispLines.AddStrings(Lines);
      FTextBmp.Canvas.Brush.Style := bsClear;
      AAFont.Effect.Assign(FText.FontEffect);
      if Fade then            // ���뵭���հ�
        CurrHeight := FadeHeight
      else
        CurrHeight := 0;
      CurrHeight := CurrHeight + Height * HeadSpace div 100; // ͷ���հ�
      FTextBmp.Canvas.Font.Assign(Font);
      CurrAlign := Alignment; // Ĭ�϶��뷽ʽ
      for I := 0 to DispLines.Count - 1 do
      begin
        CurrText := DispLines[I]; // ��ǰ�����ַ���
        if LabelEffect = leOnlyALine then
        begin
          FTextBmp.Canvas.Font.Assign(Font);
          AAFont.Effect.Assign(FText.FontEffect);
          CurrAlign := Alignment;
        end;
        Fonts.Check(CurrText, FTextBmp.Canvas.Font, AAFont.Effect); // ��������ǩ
        Labels.Check(CurrText, CurrAlign); // ����û���ǩ
        TextWidth := AAFont.TextWidth(CurrText);
        if WordWrap and (TextWidth > Width) then // �Զ�����
        begin
          MaxCol := Width * Length(CurrText) div TextWidth;
          while AAFont.TextWidth(Copy(CurrText, 1, MaxCol)) > Width do
            Dec(MaxCol);
          WrapText(CurrText, WrapLines, MaxCol);
        end
        else if CurrText <> '' then
          WrapLines.Text := CurrText
        else
          WrapLines.Text := ' ';

        for J := 0 to WrapLines.Count - 1 do
        begin
          CurrText := WrapLines[J];
          TextHeight := AAFont.TextHeight(CurrText + ' ');
          TextWidth := AAFont.TextWidth(CurrText);
          case CurrAlign of     // ���뷽ʽ
            taLeftJustify: X := 0;
            taCenter: X := (FTextBmp.Width - TextWidth) div 2;
            taRightJustify: X := FTextBmp.Width - TextWidth;
          else X := 0;
          end;
          Y := CurrHeight;      // �м��
          AAFont.TextOutput(X, Y, CurrText, FTransparent);

          CurrHeight := CurrHeight + Round(TextHeight * (1 + RowPitch / 100));
        end;
      end;

      if Assigned(OnTextReady) then //���� OnTextReady �¼�
        OnTextReady(Self);
    end;
  finally
    WrapLines.Free;
    DispLines.Free;
    EndUpdate;
  end;
end;

// ���û
procedure TCnAAScrollText.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    FScrollTimer.Enabled := FActive;
    if not FActive then
      FDelayTimer.Enabled := False;
  end;
end;

// ����ѭ����ʱ
procedure TCnAAScrollText.SetRepeatDelay(const Value: Word);
begin
  if FRepeatDelay <> Value then
  begin
    FRepeatDelay := Value;
    if FRepeatDelay <= 0 then
      FRepeatDelay := 0;
    FDelayTimer.Interval := Value;
  end;
end;

// ���ù�����ʱ
procedure TCnAAScrollText.SetScrollDelay(const Value: Word);
begin
  if FScrollDelay <> Value then
  begin
    FScrollDelay := Value;
    if FScrollDelay <= 0 then
      FScrollDelay := 0;
    FScrollTimer.Interval := FScrollDelay;
  end;
end;

// ����ÿ�ι�������
procedure TCnAAScrollText.SetScrollStep(const Value: Integer);
begin
  if FScrollStep <> Value then
  begin
    FScrollStep := Value;
  end;
end;

// ����ѭ������
procedure TCnAAScrollText.SetRepeatCount(const Value: TBorderWidth);
begin
  if FRepeatCount <> Value then
  begin
    FRepeatCount := Value;
    if FRepeatCount <= 0 then
      FRepeatCount := 0;
    Changed;
  end;
end;

// �����ı�����
procedure TCnAAScrollText.SetText(const Value: TCnScrollTextParam);
begin
  FText.Assign(Value);
end;

// ��ͷ��ʼ����
procedure TCnAAScrollText.ReStart;
begin
  FRepeatedCount := -1;
  CurrPos := 0;
end;

// ���õ�ǰλ��
procedure TCnAAScrollText.SetCurrPos(const Value: Integer);
begin
  if FCurrPos <> Value then
  begin
    FCurrPos := Value mod FTextBmp.Height;
    if FCurrPos < 0 then
      Inc(FCurrPos, FTextBmp.Height);
    Paint;
  end;
end;

{$IFNDEF FPC}
// ��С�仯��Ϣ
function TCnAAScrollText.CanResize(var NewWidth,
  NewHeight: Integer): Boolean;
begin
  if NewWidth < 20 then NewWidth := 20;
  if NewHeight < 20 then NewHeight := 20;
  Result := inherited CanResize(NewWidth, NewHeight);
end;
{$ENDIF}

// ѭ����ʱ
procedure TCnAAScrollText.OnDelayTimer(Sender: TObject);
begin
  FDelayTimer.Enabled := False;
  CurrPos := CurrPos + FScrollStep;
  if Active then
    FScrollTimer.Enabled := True;
end;

// ����Ĭ�����弯
procedure TCnAAScrollText.CreateDefFonts;
var
  FLabel: TCnFontLabel;
begin
  inherited;
  FLabel := Fonts.AddItem('Title4', '����', 22, clBlack, [fsBold], True, 2, 2);
  if Assigned(FLabel) then
  begin
    FLabel.Effect.Gradual.Enabled := True;
    FLabel.Effect.Gradual.Style := gsLeftToRight;
    FLabel.Effect.Gradual.StartColor := $00FF2200;
    FLabel.Effect.Gradual.EndColor := $002210FF;
    FLabel.Effect.Outline := True;
    FLabel.Effect.Blur := 50;
  end;
  FLabel := Fonts.AddItem('Text3', '����', 11, clBlue, [], True, 1, 1);
  if Assigned(FLabel) then
  begin
    FLabel.Effect.Gradual.Enabled := True;
    FLabel.Effect.Gradual.Style := gsTopToBottom;
    FLabel.Effect.Gradual.StartColor := $00CC3311;
    FLabel.Effect.Gradual.EndColor := $00FF22AA;
  end;
end;

// Ĭ���ı�����Ĭ�ϱ�ǩ
function TCnAAScrollText.UseDefaultLabels: Boolean;
begin
  Result := not FText.IsLinesStored;
end;

// �ؼ�������װ��
procedure TCnAAScrollText.LoadedEx;
begin
  inherited;
  Reset;
end;

{ TCnAAMarqueeText }

//--------------------------------------------------------//
// ƽ����Ļ�ı��ؼ�                                       //
//--------------------------------------------------------//

// ��ʼ��
constructor TCnAAMarqueeText.Create(AOwner: TComponent);
begin
  inherited;
  FEffect := TCnAAFontEffect.Create(Self, OnEffectChanged);
  ControlStyle := ControlStyle + [csReplicatable, csSetCaption];
  FTimer := TTimer.Create(Self);
  Height := 34;
  Width := 240;
  FScrollType := stRightToLeft;
  FActive := False;
  FSteps := 0;
  FScrollStep := 1;
  FScrollDelay := 100;
  FTimer.Enabled := FActive;
  FTimer.Interval := FScrollDelay;
  FTimer.OnTimer := OnTimer;
end;

// �ͷ�
destructor TCnAAMarqueeText.Destroy;
begin
  FEffect.Free;
  FTimer.Free;
  inherited;
end;

// ��ʱ���¼�
procedure TCnAAMarqueeText.OnTimer(Sender: TObject);
begin
  if not FTimer.Enabled or not Visible then Exit;
  Inc(FCurrentStep, FScrollStep);
  Paint;
  if FCurrentStep >= FSteps then
    FCurrentStep := 0;
end;

// ���ƻ���
procedure TCnAAMarqueeText.PaintCanvas;
var
  R: TRect;
  X, Y: Integer;
  lpPaint: tagPAINTSTRUCT;
  FMemBmp: TBitmap;
begin
  inherited;
  X := 0;
  BeginPaint(Canvas.Handle, lpPaint);
  FMemBmp := TBitmap.Create;
  try
    FMemBmp.PixelFormat := pf24bit;
    AAFont.Canvas := FMemBmp.Canvas;
    FMemBmp.Canvas.Font.Assign(Font); //����
    R := ClientRect;
    case FEffect.Layout of
      tlTop: Y := 0;
      tlCenter: Y := R.Top + (R.Bottom - R.Top - FMemBmp.Canvas.TextHeight('Pp')) div 2;
      tlBottom: Y := R.Bottom - R.Top - FMemBmp.Canvas.TextHeight(Caption);
      else Y := 0;
    end;

    case FScrollType of
      stRightToLeft: X := Width - FCurrentStep;
      stLeftToRight: X := - FTextWidth + FCurrentStep;
      stNone:
        case FEffect.Alignment of
          taCenter: X := (Width - FTextWidth) div 2;
          taLeftJustify: X := 0;
          taRightJustify: X := Width - FTextWidth;
        end;
    else X := 0;
    end;

    FMemBmp.Height := ClientHeight;
    FMemBmp.Width := ClientWidth;
    FMemBmp.Canvas.Brush.Color := Color;
    FMemBmp.Canvas.Brush.Style := bsSolid;

    if FEffect.Transparent then  // ͸��
    begin
      CopyParentImage(FMemBmp.Canvas); // ���Ƹ��ؼ�����
    end
    else if not FEffect.IsBackEmpty then
    begin                       // ���Ʊ���ͼ
      DrawBackGround(FMemBmp.Canvas, Rect(0, 0, FMemBmp.Width, FMemBmp.Height),
        FEffect.BackGround.Graphic, FEffect.BackGroundMode);
    end else
    begin                       // ��䱳��ɫ
      FMemBmp.Canvas.FillRect(ClientRect);
    end;
    FMemBmp.Canvas.Brush.Style := bsClear;
    AAFont.TextOutput(X, Y, Caption); // ƽ���������
    Bitblt(Canvas.Handle, 0, 0, Width, Height, FMemBmp.Canvas.Handle, 0, 0,
      SRCCOPY);
  finally
    FMemBmp.Free;
    EndPaint(Canvas.Handle, lpPaint);
  end;
  if Assigned(OnPainted) then
    OnPainted(Self);
end;

// ��λ
procedure TCnAAMarqueeText.Reset;
var
  Bmp: TBitmap;
  tActive: Boolean;
begin
  inherited Reset;
  tActive := Active;
  Active := False;
  Active := tActive;
  
  Bmp := TBitmap.Create;
  try
    if AutoSize and (FEffect.BackGround.Graphic <> nil) then
    begin
      Width := FEffect.BackGround.Width;
      Height := FEffect.BackGround.Height;
    end;
    Bmp.Canvas.Font.Assign(Font);
    FTextWidth := Bmp.Canvas.TextWidth(Caption);
    FSteps := FTextWidth + Width;
  finally
    Bmp.Free;
  end;
end;

// ���ù�����ʱ
procedure TCnAAMarqueeText.SetScrollDelay(Value: Word);
begin
  if FScrollDelay <> Value then
  begin
    FScrollDelay := Value;
    if FTimer <> nil then
      FTimer.Interval := FScrollDelay;
  end;
end;

// ����ÿ�ι�������
procedure TCnAAMarqueeText.SetScrollStep(Value: Word);
begin
  if FScrollStep <> Value then
  begin
    if Value < 1 then
      Value := 1;
    FScrollStep := Value;
  end;
end;

// ���û�Ծ
procedure TCnAAMarqueeText.SetActive(Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    FTimer.Enabled := FActive;
    if not FActive then
      FCurrentStep := 0;
    Invalidate;
  end;
end;

// ����������Ч
procedure TCnAAMarqueeText.SetEffect(const Value: TCnAAFontEffect);
begin
  FEffect.Assign(Value);
end;

// ���ù�������
procedure TCnAAMarqueeText.SetScrollType(Value: THoriScrollType);
begin
  if FScrollType <> Value then
  begin
    FScrollType := Value;
    FTimer.Enabled := FScrollType <> stNone;
    Invalidate;
  end;
end;

{ TCnScrollTextParam }

//--------------------------------------------------------//
// ƽ�������ı�����                                       //
//--------------------------------------------------------//

// ��ʼ��
constructor TCnScrollTextParam.Create(AOwner: TCnAAGraphicControl;
  ChangedProc: TNotifyEvent);
begin
  inherited;
  TStringList(Lines).Text := csAAScrollTextCopyRight;
  FFade := True;
  FFadeHeight := 10;
  FHeadSpace := 0;
  FTailSpace := 60;
  Alignment := taCenter;
  BackGroundMode := bmTiled;
end;

// �ͷ�
destructor TCnScrollTextParam.Destroy;
begin
  inherited;
end;

// ���õ��뵭��
procedure TCnScrollTextParam.SetFade(const Value: Boolean);
begin
  if FFade <> Value then
  begin
    FFade := Value;
    Changed;
  end;
end;

// ���õ��뵭���߶�
procedure TCnScrollTextParam.SetFadeHeight(const Value: Integer);
begin
  if FFadeHeight <> Value then
  begin
    FFadeHeight := Value;
    Changed;
  end;
end;

// ����ͷ���հ�
procedure TCnScrollTextParam.SetHeadSpace(const Value: Integer);
begin
  if FHeadSpace <> Value then
  begin
    FHeadSpace := Value;
    if FHeadSpace < 0 then
      FHeadSpace := 0;
    if FHeadSpace > 150 then
      FHeadSpace := 150;
    Changed;
  end;
end;

// ����β���հ�
procedure TCnScrollTextParam.SetTailSpace(const Value: Integer);
begin
  if FTailSpace <> Value then
  begin
    FTailSpace := Value;
    if FTailSpace < 0 then
      FTailSpace := 0;
    if FTailSpace > 150 then
      FTailSpace := 150;
    Changed;
  end;
end;

// �ı������Ƿ�洢
function TCnScrollTextParam.IsLinesStored: Boolean;
begin
  Result := Lines.Text <> csAAScrollTextCopyRight;
end;

{ TCnAAFadeText }

//--------------------------------------------------------//
// ƽ����Ч�����ı��ؼ�                                   //
//--------------------------------------------------------//

// ����
constructor TCnAAFadeText.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOpaque, csReplicatable];
  FTextBmp := TBitmap.Create;
  FTextBmp.PixelFormat := pf24bit;
  FInBmp := TBitmap.Create;
  FInBmp.PixelFormat := pf24bit;
  FOutBmp := TBitmap.Create;
  FOutBmp.PixelFormat := pf24bit;
  FFadeTimer := TTimer.Create(Self);
  FFadeTimer.Interval := 25;
  FFadeTimer.Enabled := False;
  FFadeTimer.OnTimer := OnFadeTimer;
  FDelayTimer := TTimer.Create(Self);
  FDelayTimer.Enabled := False;
  FDelayTimer.OnTimer := OnDelayTimer;
  FText := TCnFadeTextParam.Create(Self, OnLabelChanged);
  FLineIndex := -1;
  FFadeProgress := 0;
  FRepeatCount := 0;
  FRepeatedCount := 0;
  FActive := True;
  Color := clWhite;
  LastText := '';
  CurrText := '';
  FNewProg := 0;
  SetBounds(0, 0, 240, 34);
end;

// ����Ĭ�������ǩ
procedure TCnAAFadeText.CreateDefFonts;
var
  FLabel: TCnFontLabel;
begin
  inherited;
  FLabel := Fonts.AddItem('Title4', '����', 22, clBlack, [], True, 2, 2);
  if Assigned(FLabel) then
  begin
    FLabel.Effect.Gradual.Enabled := True;
    FLabel.Effect.Gradual.Style := gsLeftToRight;
    FLabel.Effect.Gradual.StartColor := $00FF2200;
    FLabel.Effect.Gradual.EndColor := $002210FF;
    FLabel.Effect.Outline := True;
    FLabel.Effect.Blur := 50;
  end;
  FLabel := Fonts.AddItem('Text3', '����', 11, clBlue, [], True, 1, 1);
  if Assigned(FLabel) then
  begin
    FLabel.Effect.Gradual.Enabled := True;
    FLabel.Effect.Gradual.Style := gsTopToBottom;
    FLabel.Effect.Gradual.StartColor := $00CC8811;
    FLabel.Effect.Gradual.EndColor := $00FF22AA;
  end;
end;

// �ͷ�
destructor TCnAAFadeText.Destroy;
begin
  FText.Free;
  FDelayTimer.Free;
  FFadeTimer.Free;
  FOutBmp.Free;
  FInBmp.Free;
  FTextBmp.Free;
  inherited;
end;

// ���ƽ���ͼ
procedure TCnAAFadeText.DrawFadeBmp(AText: string; Bmp: TBitmap);
var
  OffPoint: TPoint;
  th, tw: Integer;
begin
  AAFont.Canvas := Bmp.Canvas;
  if Text.LabelEffect = leOnlyALine then
  begin
    Bmp.Canvas.Font.Assign(Font);
    AAFont.Effect.Assign(Text.FontEffect);
    CurrAlign := Text.Alignment;
  end;
  Fonts.Check(AText, Bmp.Canvas.Font, AAFont.Effect); // ��������ǩ
  Labels.Check(AText, CurrAlign); // ����û���ǩ
  th := AAFont.TextHeight(AText); // �ı��߶�
  tw := AAFont.TextWidth(AText);  // �ı����

  case CurrAlign of               // ˮƽ���뷽ʽ
    taLeftJustify: OffPoint.X := 0;
    taRightJustify: OffPoint.X := ClientWidth - tw;
    taCenter: OffPoint.X := (ClientWidth - tw) div 2;
  end;
  case Text.Layout of         //��ֱ���뷽ʽ
    tlTop: OffPoint.Y := 0;
    tlCenter: OffPoint.Y := (ClientHeight - th) div 2;
    tlBottom: OffPoint.Y := ClientHeight - th;
  end;

  Bmp.Height := ClientHeight;
  Bmp.Width := ClientWidth;
  Bmp.Canvas.Brush.Color := Color;
  Bmp.Canvas.Brush.Style := bsSolid;
  if Text.Transparent then    //͸��
  begin
    CopyParentImage(Bmp.Canvas); //���Ƹ��ؼ�����
  end else if not Text.IsBackEmpty then
  begin                       //���Ʊ���ͼ
    DrawBackGround(Bmp.Canvas, Rect(0, 0, Bmp.Width, Bmp.Height),
      Text.BackGround.Graphic, Text.BackGroundMode);
  end else
  begin                       //��䱳��ɫ
    Bmp.Canvas.FillRect(ClientRect);
  end;
  Bmp.Canvas.Brush.Style := bsClear;
  AAFont.TextOutput(OffPoint.X, OffPoint.Y, AText); //ƽ���������
end;

//������ָ����
procedure TCnAAFadeText.FadeTo(Line: Integer);
begin
  if Text.Lines.Count <= 0 then
    Exit;
  if Line < 0 then
    Line := 0;
  if Line > Text.Lines.Count - 1 then
  begin
    Line := 0;
    Inc(FRepeatedCount);
    if (FRepeatCount > 0) and (FRepeatedCount >= FRepeatCount) then
    begin
      Active := False;
      FRepeatedCount := 0;
      FLineIndex := -1;
      FadeToStr('');
      if Assigned(OnComplete) then
        OnComplete(Self);
      Exit;
    end;
  end;
  FadeToStr(Text.Lines[Line]);
  FLineIndex := Line;
end;

// ��������һ��
procedure TCnAAFadeText.FadeToNext;
begin
  FadeTo(LineIndex + 1);
end;

// ������ָ���ı�
procedure TCnAAFadeText.FadeToStr(AText: string);
begin
  FOutBmp.Assign(FTextBmp);
  DrawFadeBmp(AText, FInBmp);
  LastText := CurrText;
  CurrText := AText;
  FFadeProgress := 0;
  FFadeTimer.Enabled := False;
  FFadeTimer.Enabled := True;
  if FDelayTimer.Enabled then
  begin
    FDelayTimer.Enabled := False;
    FDelayTimer.Enabled := True;
  end;
end;

// ������װ��
procedure TCnAAFadeText.LoadedEx;
begin
  inherited;
  CurrAlign := Text.Alignment;
  Reset;
  FRepeatedCount := 0;
  FDelayTimer.Enabled := FActive;
  if FActive then
    OnDelayTimer(Self);
end;

// �����л��ı���ʱ�¼�
procedure TCnAAFadeText.OnDelayTimer(Sender: TObject);
begin
  FadeToNext;
end;


// �������̶�ʱ�¼�
procedure TCnAAFadeText.OnFadeTimer(Sender: TObject);
begin
  if Abs(FNewProg - FadeProgress) > 1 then
    FNewProg := FadeProgress;
  FNewProg := FNewProg + csMaxProgress * FFadeTimer.Interval div Text.FadeDelay;
  if FNewProg > csMaxProgress then
  begin
    FNewProg := csMaxProgress;
    FFadeTimer.Enabled := False;
  end;
  FadeProgress := Round(FNewProg);
end;

// ���ƿؼ�����
procedure TCnAAFadeText.PaintCanvas;
begin
  inherited;
  if Text.Transparent then
  begin                       // ͸���������ػ�
    if FadeProgress = 0 then
      DrawFadeBmp(CurrText, FTextBmp)
    else begin
      DrawFadeBmp(LastText, FOutBmp);
      DrawFadeBmp(CurrText, FInBmp);
    end;
  end;
  if FadeProgress <> 0 then   // ������
    Blend(FTextBmp, FOutBmp, FInBmp, FFadeProgress);
  Bitblt(Canvas.Handle, 0, 0, Width, Height, FTextBmp.Canvas.Handle, 0, 0,
    SRCCOPY);
  if Assigned(OnPainted) then
    OnPainted(Self);
end;

// ������ʾ
procedure TCnAAFadeText.Reset;
begin
  if FadeProgress = 0 then
    DrawFadeBmp(CurrText, FTextBmp)
  else begin
    DrawFadeBmp(LastText, FOutBmp);
    DrawFadeBmp(CurrText, FInBmp);
    Blend(FTextBmp, FOutBmp, FInBmp, FFadeProgress);
  end;
  inherited;
end;

// ���û�Ծ
procedure TCnAAFadeText.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    FDelayTimer.Enabled := FActive;
    if FActive then
    begin
      FRepeatedCount := 0;
      OnDelayTimer(Self);
    end;
  end;
end;

// ���ý�������
procedure TCnAAFadeText.SetFadeProgress(const Value: TProgress);
begin
  if FFadeProgress <> Value then
  begin
    FFadeProgress := Value;
    Paint;
  end;
end;

// ���õ�ǰ��
procedure TCnAAFadeText.SetLineIndex(const Value: Integer);
begin
  if FLineIndex <> Value then
  begin
    FadeTo(FLineIndex);
  end;
end;

// ������ѭ������
procedure TCnAAFadeText.SetRepeatCount(const Value: TBorderWidth);
begin
  if FRepeatCount <> Value then
  begin
    FRepeatCount := Value;
    if FRepeatedCount >= FRepeatCount then
  end;
end;

// �����ı�
procedure TCnAAFadeText.SetText(const Value: TCnFadeTextParam);
begin
  FText.Assign(Value);
end;

// ��Ĭ���ı�ʱ����Ĭ�ϱ�ǩ
function TCnAAFadeText.UseDefaultLabels: Boolean;
begin
  Result := not FText.IsLinesStored;
end;

{ TCnFadeTextParam }

//--------------------------------------------------------//
// ƽ����Ч�����ı�����                                   //
//--------------------------------------------------------//

//��ֵ
procedure TCnFadeTextParam.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TCnFadeTextParam then
  begin
    FFadeDelay := TCnFadeTextParam(Source).FadeDelay;
    LineDelay := TCnFadeTextParam(Source).LineDelay;
  end;
end;

// ����
constructor TCnFadeTextParam.Create(AOwner: TCnAAGraphicControl;
  ChangedProc: TNotifyEvent);
begin
  inherited;
  TStringList(Lines).Text := csAAFadeTextCopyRight;
  FadeDelay := 600;
  LineDelay := 3000;
  Alignment := taCenter;
  Layout := tlCenter;
end;

// �ͷ�
destructor TCnFadeTextParam.Destroy;
begin
  inherited;
end;

// ȡ����ʱ
function TCnFadeTextParam.GetLineDelay: Cardinal;
begin
  Result := TCnAAFadeText(Owner).FDelayTimer.Interval;
end;

// ȡͼ��߶�
function TCnAAScrollText.GetBmpHeight: Integer;
begin
  Result := FTextBmp.Height;
end;

// �洢�ı�
function TCnFadeTextParam.IsLinesStored: Boolean;
begin
  Result := Lines.Text <> csAAFadeTextCopyRight;
end;

// ���ý�����ʱ
procedure TCnFadeTextParam.SetFadeDelay(const Value: Cardinal);
begin
  if FFadeDelay <> Value then
  begin
    FFadeDelay := Value;
    if FFadeDelay > LineDelay - 200 then
      FFadeDelay := LineDelay - 200;
    if FFadeDelay < 50 then
      FFadeDelay := 50;
  end;
end;

// ��������ʱ
procedure TCnFadeTextParam.SetLineDelay(const Value: Cardinal);
var
  T: Cardinal;
begin
  T := Value;
  if T < FFadeDelay + 200 then
    T := FFadeDelay + 200;
  TCnAAFadeText(Owner).FDelayTimer.Interval := T;
end;

procedure TCnAAScrollText.SetTransparent(const Value: Boolean);
begin
  if Value <> FTransparent then
  begin
    FTransparent := Value;
    if not (csLoading in ComponentState) then
      Reset;
  end;
end;

end.
