{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2014 CnPack 开发组                       }
{                   ------------------------------------                       }
{                                                                              }
{            本开发包是开源的自由软件，您可以遵照 CnPack 的发布协议来修        }
{        改和重新发布这一程序。                                                }
{                                                                              }
{            发布这一开发包的目的是希望它有用，但没有任何担保。甚至没有        }
{        适合特定目的而隐含的担保。更详细的情况请参阅 CnPack 发布协议。        }
{                                                                              }
{            您应该已经和开发包一起收到一份 CnPack 发布协议的副本。如果        }
{        还没有，可访问我们的网站：                                            }
{                                                                              }
{            网站地址：http://www.cnpack.org                                   }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnImage;
{* |<PRE>
================================================================================
* 软件名称：界面控件包
* 单元名称：调试用界面控件TCnImage、TCnPaintBox单元
* 单元作者：周劲羽 (zjy@cnpack.org)
* 备    注：该单元当前仅为内部参考测试用
* 开发平台：PWin98SE + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2002.04.09 V1.0
*               重新修正，设置单元版本号
*           2002.02.01 V0.01Demo
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CnVCLBase, CnGraphics, Math;

type

{ TCnPaintBox }

  TCnPaintBox = class(TCnGraphicControl)
  {* 用于测试CnPack图像类基础库的控件，功能类似于TPaintBox}
  private
    function GetFont: TCnFont;
    procedure SetFont(const Value: TCnFont);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Repaint; override;
    {* 重绘制画布}
    property Face;
  published
    property Align;
    property Anchors;
    property AlphaBlend;
    property AlphaBlendValue;
    property Color;
    {* 控件背景颜色}
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property Font: TCnFont read GetFont write SetFont;
    {* 平滑特效字体属性}
    property PopupMenu;
    property ShowHint;
    property FullPaint;
    property Transparent;
    property Visible;
    property OnClick;
    property OnPaint;
    {* 控件画布重绘事件，在该事件在对Face进行绘制}
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

{ TCnImage }

  TCnImage = class(TCnGraphicControl)
  {* 用于测试CnPack图像类基础库的控件，功能类似于TImage}
  private
    { Private declarations }
    FDrawStyle: TCnDrawMode;
    FBitmap: TCnBitmap;
    function GetSmoothFilter: Boolean;
    procedure SetDrawStyle(const Value: TCnDrawMode);
    procedure SetSmoothFilter(const Value: Boolean);
    procedure SetBitmap(const Value: TCnBitmap);
    function GetFont: TCnFont;
    procedure SetFont(const Value: TCnFont);
  protected
    { Protected declarations }
    procedure Paint; override;
    procedure SetTransparent(const Value: Boolean); override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure OnChildChange(Sender: TObject); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Face;
  published
    { Published declarations }
    property Align;
    property Anchors;
    property AutoSize;
    property AlphaBlend;
    property AlphaBlendValue;
    property Color;
    property DrawStyle: TCnDrawMode read FDrawStyle write SetDrawStyle default dmDraw;
    {* 图像显示模式}
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FullPaint;
    property ParentShowHint;
    property Bitmap: TCnBitmap read FBitmap write SetBitmap;
    {* 存储的图像}
    property Font: TCnFont read GetFont write SetFont;
    {* 平滑特效字体属性}
    property PopupMenu;
    property ShowHint;
    property SmoothFilter: Boolean read GetSmoothFilter write SetSmoothFilter default
      True;
    {* 决定缩放绘制时是否使用二次插值算法进行平滑处理}
    property Transparent;
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
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

{ TCnPaintBox }

constructor TCnPaintBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  Width := 105;
  Height := 105;
end;

function TCnPaintBox.GetFont: TCnFont;
begin
  Result := Face.Font;
end;

procedure TCnPaintBox.Paint;
begin
  if (Height > 0) and (Width > 0) and (csDesigning in ComponentState) then
    Face.FrameRect(Rect(0, 0, Width - 1, Height - 1), clBlack);
  if Assigned(OnPaint) then OnPaint(Self);
end;

procedure TCnPaintBox.Repaint;
begin
  inherited;
  Changed;
end;

procedure TCnPaintBox.SetFont(const Value: TCnFont);
begin
  Face.Font.Assign(Value);
end;

{ TCnImage }

constructor TCnImage.Create(AOwner: TComponent);
begin
  inherited;
  FBitmap := TCnBitmap.Create(OnChildChange);
  FDrawStyle := dmDraw;
  SmoothFilter := True;
  Height := 105;
  Width := 105;
end;

destructor TCnImage.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

function TCnImage.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := inherited CanAutoSize(NewWidth, NewHeight);
  if Result and not FBitmap.Empty then
  begin
    NewWidth := FBitmap.Width;
    NewHeight := FBitmap.Height;
  end;
end;

function TCnImage.GetSmoothFilter: Boolean;
begin
  Result := Face.SmoothFilter;
end;

procedure TCnImage.Paint;
begin
  inherited;
  if (Height > 0) and (Width > 0) then
  begin
    if csDesigning in ComponentState then
      Face.FrameRect(Rect(0, 0, Width - 1, Height - 1), clBlack);
    if not FBitmap.Empty then
      Face.DrawMode(FBitmap, FDrawStyle);
  end;
end;

procedure TCnImage.SetBitmap(const Value: TCnBitmap);
begin
  FBitmap.Assign(Value);
  if AutoSize then SetBounds(Left, Top, Width, Height);
end;

procedure TCnImage.SetDrawStyle(const Value: TCnDrawMode);
begin
  if FDrawStyle <> Value then
  begin
    FDrawStyle := Value;
    Changed;
  end;
end;

procedure TCnImage.SetSmoothFilter(const Value: Boolean);
begin
  Face.SmoothFilter := Value;
  Changed;
end;

procedure TCnImage.SetTransparent(const Value: Boolean);
begin
  BeginUpdate;
  try
    inherited;
    FBitmap.Transparent := Value;
  finally
    EndUpdate;
  end;
end;

function TCnImage.GetFont: TCnFont;
begin
  Result := Face.Font;
end;

procedure TCnImage.SetFont(const Value: TCnFont);
begin
  Face.Font.Assign(Value);
end;

procedure TCnImage.OnChildChange(Sender: TObject);
begin
  inherited;
  if (Sender = FBitmap) and AutoSize then
    SetBounds(Left, Top, Width, Height);
end;

end.

