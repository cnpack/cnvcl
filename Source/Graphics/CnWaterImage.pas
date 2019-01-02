{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2019 CnPack 开发组                       }
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

unit CnWaterImage;
{* |<PRE>
================================================================================
* 软件名称：界面控件包
* 单元名称：水波效果图像控件
* 单元作者：周劲羽 (zjy@cnpack.org)
* 备    注：
* 开发平台：PWinXP SP2 + Delphi 5.01
* 兼容测试：
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2008.11.17 V1.1
*               笑三少增加控制水滴初始半径与荡漾快慢的两个属性
*           2005.11.22 V1.0
*               创建控件
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, ExtCtrls,
  Math, CnWaterEffect;

const
  csDefRandomDelay = 800;
  csDefRandomBlob = 500;
  csDefTrackBlob = 100;
  csDefClickBlob = 250;
  csDefRadius = 1;
  csDefInterval = 50;

type

{ TCnWaterImage }

  TCnRenderEvent = procedure (Sender: TObject; ABitmap: TBitmap) of object;

  TCnWaterImage = class(TGraphicControl)
  {* 水波效果图像控件 }
  private
    FPicture: TPicture;
    FTimer: TTimer;
    FSrcBmp: TBitmap;
    FDstBmp: TBitmap;
    FWater: TCnWaterEffect;
    FDrawing: Boolean;
    FRadius : Integer;
    FRandomDelay: Integer;
    FTrackBlob: Integer;
    FClickBlob: Integer;
    FRandomBlob: Integer;
    FOnAfterRender: TCnRenderEvent;
    FOnBeforeRender: TCnRenderEvent;
    procedure PictureChanged(Sender: TObject);
    procedure SetPicture(Value: TPicture);
    procedure UpdateWaterData;
    procedure OnTimer(Sender: TObject);
    function GetCanvas: TCanvas;
    function GetDamping: TWaterDamping;
    procedure SetDamping(const Value: TWaterDamping);
    function GetInterval: Cardinal;
    procedure SetInterval(Value: Cardinal);
  protected
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Loaded; override;
    procedure Resize; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearWater;
    {* 清空画面上的水滴效果 }
    procedure Blob(x, y: Integer; ARadius, AHeight: Integer);
    {* 在画面上产生一个水滴效果。x, y 为坐标，如果为 -1 表示随机点。ARadius 和
      AHeight 为初始半径和效果幅度 }
    property Canvas: TCanvas read GetCanvas;
    {* 画布属性，只在 OnBeforeRender 事件中有用 }
  published
    property Align;
    property Anchors;
    property AutoSize;
    property ClickBlob: Integer read FClickBlob write FClickBlob default csDefClickBlob;
    {* 点击画面时产生的水滴效果幅度，0 表示禁用 }
    property Constraints;
    property Damping: TWaterDamping read GetDamping write SetDamping default csDefDamping;
    {* 水滴阻尼系数 }
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Radius : Integer read FRadius write FRadius default csDefRadius;
    {* 水波初始半径，默认为 1 }
    property Interval : Cardinal read GetInterval write SetInterval default csDefInterval;
    {* 波纹荡漾快慢的间隔时间，单位毫秒，默认 50 }
    property ParentShowHint;
    property Picture: TPicture read FPicture write SetPicture;
    {* 背景图像 }
    property PopupMenu;
    property RandomBlob: Integer read FRandomBlob write FRandomBlob default csDefRandomBlob;
    {* 随机产生的水滴最大幅度，0 表示禁用 }
    property RandomDelay: Integer read FRandomDelay write FRandomDelay default csDefRandomDelay;
    {* 随机产生水滴的延时 }
    property ShowHint;
    property TrackBlob: Integer read FTrackBlob write FTrackBlob default csDefTrackBlob;
    {* 鼠标移动轨迹下水滴的幅度，0 表示禁用 }
    property Visible;
    property OnAfterRender: TCnRenderEvent read FOnAfterRender write FOnAfterRender;
    {* 画面绘制后事件 }
    property OnBeforeRender: TCnRenderEvent read FOnBeforeRender write FOnBeforeRender;
    {* 画面绘制前事件 }
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

{ TCnWaterImage }

constructor TCnWaterImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable, csOpaque];
  FPicture := TPicture.Create;
  FPicture.OnChange := PictureChanged;
  FTimer := TTimer.Create(Self);
  FTimer.Interval := csDefInterval;
  FTimer.OnTimer := OnTimer;
  FTimer.Enabled := True;
  FSrcBmp := TBitmap.Create;
  FDstBmp := TBitmap.Create;
  FWater := TCnWaterEffect.Create;
  FRandomDelay := csDefRandomDelay;
  FRandomBlob := csDefRandomBlob;
  FTrackBlob := csDefTrackBlob;
  FClickBlob := csDefClickBlob;
  Height := 105;
  Width := 105;
end;

destructor TCnWaterImage.Destroy;
begin
  FPicture.Free;
  FTimer.Free;
  FSrcBmp.Free;
  FDstBmp.Free;
  FWater.Free;
  inherited Destroy;
end;

procedure TCnWaterImage.Paint;
var
  Save: Boolean;
begin
  Canvas.Lock;
  if csDesigning in ComponentState then
    with inherited Canvas do
    begin
      Pen.Style := psDash;
      Brush.Style := bsClear;
      Rectangle(0, 0, Width, Height);
    end;
  Save := FDrawing;
  FDrawing := True;
  try
    if Picture.Graphic <> nil then
      with inherited Canvas do
        Draw(0, 0, FDstBmp);
  finally
    Canvas.UnLock;
    FDrawing := Save;
  end;
end;

function TCnWaterImage.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := True;
  if not (csDesigning in ComponentState) or (Picture.Width > 0) and
    (Picture.Height > 0) then
  begin
    if Align in [alNone, alLeft, alRight] then
      NewWidth := Picture.Width;
    if Align in [alNone, alTop, alBottom] then
      NewHeight := Picture.Height;
  end;
end;

procedure TCnWaterImage.Resize;
begin
  UpdateWaterData;
  inherited;
end;

procedure TCnWaterImage.UpdateWaterData;
begin
  if [csLoading, csDestroying] * ComponentState = [] then
  begin
    FDstBmp.Canvas.Lock;
    FSrcBmp.Canvas.lock;
    FSrcBmp.Width := Width;
    FSrcBmp.Height := Height;
    FSrcBmp.PixelFormat := pf24bit;

    FDstBmp.Width := Width;
    FDstBmp.Height := Height;
    FDstBmp.PixelFormat := pf24bit;

    FWater.SetSize(Width, Height);

    if Picture.Graphic <> nil then
    begin
      FSrcBmp.Canvas.StretchDraw(ClientRect, Picture.Graphic);
      FDstBmp.Assign(FSrcBmp);
    end;
    FDstBmp.Canvas.UnLock;
    FSrcBmp.Canvas.Unlock;
  end;    
end;

procedure TCnWaterImage.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if PtInRect(Rect(0, 0, Width, Height), Point(X, Y)) then
  begin
    if ssLeft in Shift then
      Blob(X, Y, FRadius, FClickBlob)
    else
      Blob(X, Y, FRadius, FTrackBlob);
  end;
end;

procedure TCnWaterImage.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then
    Blob(X, Y, FRadius, FClickBlob);
end;

procedure TCnWaterImage.PictureChanged(Sender: TObject);
begin
  if AutoSize and (Picture.Width > 0) and (Picture.Height > 0) then
    SetBounds(Left, Top, Picture.Width, Picture.Height);
  UpdateWaterData;
  if not FDrawing then Invalidate;
end;

procedure TCnWaterImage.OnTimer(Sender: TObject);
var
  Bmp: TBitmap;
begin
  if Enabled and (Picture.Graphic <> nil) then
  begin
    if (FRandomDelay > 0) and (FRandomBlob > 0) then
    begin
      if Random(Ceil(FRandomDelay / Integer(FTimer.Interval)) + 1) = 0 then
        Blob(-1, -1, Random(2) + 1, Random(FRandomBlob) + 100);
    end;
    
    if Assigned(FOnBeforeRender) then
    begin
      Bmp := TBitmap.Create;
      try
        Bmp.Assign(FSrcBmp);
        FOnBeforeRender(Self, Bmp);
        FWater.Render(Bmp, FDstBmp);
      finally
        Bmp.Free;
      end;                      
    end
    else
      FWater.Render(FSrcBmp, FDstBmp);

    if Assigned(FOnAfterRender) then
      FOnAfterRender(Self, FDstBmp);

    Invalidate;
  end;    
end;

procedure TCnWaterImage.Blob(x, y, ARadius, AHeight: Integer);
begin
  FWater.Blob(x, y, ARadius, AHeight);
end;

procedure TCnWaterImage.ClearWater;
begin
  FWater.ClearWater;
end;

function TCnWaterImage.GetCanvas: TCanvas;
begin
  Result := FDstBmp.Canvas;
end;

function TCnWaterImage.GetDamping: TWaterDamping;
begin
  Result := FWater.Damping;
end;

function TCnWaterImage.GetInterval: Cardinal;
begin
  Result := FTimer.Interval;
end;

procedure TCnWaterImage.SetDamping(const Value: TWaterDamping);
begin
  FWater.Damping := Value;
end;

procedure TCnWaterImage.SetInterval(Value: Cardinal);
begin
  FTimer.Interval := Value;
end;

procedure TCnWaterImage.SetPicture(Value: TPicture);
begin
  FPicture.Assign(Value);
end;

procedure TCnWaterImage.Loaded;
begin
  inherited;
  UpdateWaterData;
end;

end.

