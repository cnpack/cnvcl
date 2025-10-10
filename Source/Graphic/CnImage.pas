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

unit CnImage;
{* |<PRE>
================================================================================
* ������ƣ�����ؼ���
* ��Ԫ���ƣ������ý���ؼ�TCnImage��TCnPaintBox��Ԫ
* ��Ԫ���ߣ��ܾ��� (zjy@cnpack.org)
* ��    ע���õ�Ԫ��ǰ��Ϊ�ڲ��ο�������
* ����ƽ̨��PWin98SE + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2002.04.09 V1.0
*               �������������õ�Ԫ�汾��
*           2002.02.01 V0.01Demo
*               ������Ԫ
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
  {* ���ڲ���CnPackͼ���������Ŀؼ�������������TPaintBox}
  private
    function GetFont: TCnFont;
    procedure SetFont(const Value: TCnFont);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Repaint; override;
    {* �ػ��ƻ���}
    property Face;
  published
    property Align;
    property Anchors;
    property AlphaBlend;
    property AlphaBlendValue;
    property Color;
    {* �ؼ�������ɫ}
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property Font: TCnFont read GetFont write SetFont;
    {* ƽ����Ч��������}
    property PopupMenu;
    property ShowHint;
    property FullPaint;
    property Transparent;
    property Visible;
    property OnClick;
    property OnPaint;
    {* �ؼ������ػ��¼����ڸ��¼��ڶ�Face���л���}
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
  {* ���ڲ��� CnPack ͼ���������Ŀؼ������������� TImage}
  private
    FDrawStyle: TCnDrawMode;
    FBitmap: TCnBitmap;
    function GetSmoothFilter: Boolean;
    procedure SetDrawStyle(const Value: TCnDrawMode);
    procedure SetSmoothFilter(const Value: Boolean);
    procedure SetBitmap(const Value: TCnBitmap);
    function GetFont: TCnFont;
    procedure SetFont(const Value: TCnFont);
  protected
    procedure Paint; override;
    procedure SetTransparent(const Value: Boolean); override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure OnChildChange(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Face;
  published
    property Align;
    property Anchors;
    property AutoSize;
    property AlphaBlend;
    property AlphaBlendValue;
    property Color;
    property DrawStyle: TCnDrawMode read FDrawStyle write SetDrawStyle default dmDraw;
    {* ͼ����ʾģʽ}
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FullPaint;
    property ParentShowHint;
    property Bitmap: TCnBitmap read FBitmap write SetBitmap;
    {* �洢��ͼ��}
    property Font: TCnFont read GetFont write SetFont;
    {* ƽ����Ч��������}
    property PopupMenu;
    property ShowHint;
    property SmoothFilter: Boolean read GetSmoothFilter write SetSmoothFilter default
      True;
    {* �������Ż���ʱ�Ƿ�ʹ�ö��β�ֵ�㷨����ƽ������}
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

