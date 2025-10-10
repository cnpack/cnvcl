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

unit CnWaterImage;
{* |<PRE>
================================================================================
* ������ƣ�����ؼ���
* ��Ԫ���ƣ�ˮ��Ч��ͼ��ؼ�
* ��Ԫ���ߣ��ܾ��� (zjy@cnpack.org)
* ��    ע��
* ����ƽ̨��PWinXP SP2 + Delphi 5.01
* ���ݲ��ԣ�
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2008.11.17 V1.1
*               Ц�������ӿ���ˮ�γ�ʼ�뾶�뵴����������������
*           2005.11.22 V1.0
*               �����ؼ�
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

{$IFNDEF FPC}
{$IFDEF SUPPORT_32_AND_64}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
{$ENDIF}
  TCnWaterImage = class(TGraphicControl)
  {* ˮ��Ч��ͼ��ؼ� }
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
    {* ��ջ����ϵ�ˮ��Ч�� }
    procedure Blob(x, y: Integer; ARadius, AHeight: Integer);
    {* �ڻ����ϲ���һ��ˮ��Ч����x, y Ϊ���꣬���Ϊ -1 ��ʾ����㡣ARadius ��
      AHeight Ϊ��ʼ�뾶��Ч������ }
    property Canvas: TCanvas read GetCanvas;
    {* �������ԣ�ֻ�� OnBeforeRender �¼������� }
  published
    property Align;
    property Anchors;
    property AutoSize;
    property ClickBlob: Integer read FClickBlob write FClickBlob default csDefClickBlob;
    {* �������ʱ������ˮ��Ч�����ȣ�0 ��ʾ���� }
    property Constraints;
    property Damping: TWaterDamping read GetDamping write SetDamping default csDefDamping;
    {* ˮ������ϵ�� }
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Radius : Integer read FRadius write FRadius default csDefRadius;
    {* ˮ����ʼ�뾶��Ĭ��Ϊ 1 }
    property Interval : Cardinal read GetInterval write SetInterval default csDefInterval;
    {* ���Ƶ��������ļ��ʱ�䣬��λ���룬Ĭ�� 50 }
    property ParentShowHint;
    property Picture: TPicture read FPicture write SetPicture;
    {* ����ͼ�� }
    property PopupMenu;
    property RandomBlob: Integer read FRandomBlob write FRandomBlob default csDefRandomBlob;
    {* ���������ˮ�������ȣ�0 ��ʾ���� }
    property RandomDelay: Integer read FRandomDelay write FRandomDelay default csDefRandomDelay;
    {* �������ˮ�ε���ʱ }
    property ShowHint;
    property TrackBlob: Integer read FTrackBlob write FTrackBlob default csDefTrackBlob;
    {* ����ƶ��켣��ˮ�εķ��ȣ�0 ��ʾ���� }
    property Visible;
    property OnAfterRender: TCnRenderEvent read FOnAfterRender write FOnAfterRender;
    {* ������ƺ��¼� }
    property OnBeforeRender: TCnRenderEvent read FOnBeforeRender write FOnBeforeRender;
    {* �������ǰ�¼� }
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

