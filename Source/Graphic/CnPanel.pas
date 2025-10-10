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

unit CnPanel;
{* |<PRE>
================================================================================
* ������ƣ�����ؼ���
* ��Ԫ���ƣ�ʵ����͸��Ч���� CnPanel
* ��Ԫ���ߣ�CnPack ������ (master@cnpack.org)
* ��    ע���õ�Ԫ��ǰ��Ϊ�ڲ��ο�������
* ����ƽ̨��PWin98SE + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2009.07.18 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Windows, {$IFDEF FPC} LCLType, {$ENDIF} Controls, Messages, ExtCtrls, Graphics;

type
{$IFNDEF FPC}
{$IFDEF SUPPORT_32_AND_64}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
{$ENDIF}
  TCnPanel = class(TPanel)
  private
    FBuffer: TBitmap;
    FTransparent: Boolean;
    FTransParentChanged: TNotifyEvent;
    FTransParentChanging: TNotifyEvent;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure WMWindowPosChanging(var Message: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
    procedure SetTransparent(const Value: Boolean);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure Loaded; override;
    procedure CreateWnd; override;

    procedure DoTransparentChanging; virtual;
    procedure DoTransparentChanged; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Transparent: Boolean read FTransparent write SetTransparent;
    {* �Ƿ�͸����͸��ʱ����ʾ���µ�����}
    property TransParentChanging: TNotifyEvent read FTransParentChanging write FTransParentChanging;
    property TransParentChanged: TNotifyEvent read FTransParentChanged write FTransParentChanged;
  end;

implementation

constructor TCnPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBuffer := TBitmap.Create;
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csDoubleClicks, csReplicatable];

  Width := 200;
  Height := 140;

{$IFNDEF FPC}
  ParentCtl3d := False;
  Ctl3D := False;
{$ENDIF}
  ParentColor := False;
  Color := clBtnFace;
end;

destructor TCnPanel.Destroy;
begin
  FBuffer.Free;
  inherited Destroy;
end;

procedure TCnPanel.Loaded;
begin
  inherited;

end;

procedure TCnPanel.CreateWnd;
var
  ES: Cardinal;
begin
  inherited;
  if FTransparent and not (csDesigning in ComponentState) then
  begin
    ES := GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_TRANSPARENT;
    SetWindowLong(Handle, GWL_EXSTYLE, ES);
  end;
end;

procedure TCnPanel.Paint;
const
  Alignments: array[TAlignment] of LongInt = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  R: TRect;
  FontHeight: Integer;
  Flags: LongInt;
begin
  if FTransparent and not (csDesigning in ComponentState) then // ����ڲ���ʾ͸��Ч��
  begin
    R := GetClientRect();
    FBuffer.Width := Width;
    FBuffer.Height := Height;
    FBuffer.Canvas.Brush.Style := bsSolid;
    FBuffer.Canvas.Brush.Color := Color;
    FBuffer.Canvas.FillRect(R);

    StretchBlt(FBuffer.Canvas.Handle, 0, 0, Width, Height,
               Canvas.Handle, 0, 0, Width, Height, cmSrcCopy);

{$IFNDEF FPC}
    if Ctl3D then DrawEdge(FBuffer.Canvas.Handle, R, BDR_RAISEDINNER, BF_RECT);
{$ENDIF}

    FBuffer.Canvas.Pen.Mode := pmCopy;
    FBuffer.Canvas.Pen.Style := psSolid;
    Canvas.Draw(0, 0, FBuffer);

    with Canvas do
    begin
      Brush.Style := bsClear;
      Font := Self.Font;
      FontHeight := TextHeight('C');
      R.Top := ((R.Bottom + R.Top) - FontHeight) div 2;
      R.Bottom := R.Top + FontHeight;
      Flags := DT_EXPANDTABS or DT_VCENTER or Alignments[Alignment];
{$IFNDEF FPC}
      Flags := DrawTextBiDiModeFlags(Flags);
{$ENDIF}
      DrawText(Handle, PChar(Caption), -1, R, Flags);
    end;
  end
  else
    inherited;
end;

procedure TCnPanel.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  // Params.ExStyle := Params.ExStyle + WS_EX_TRANSPARENT;
end;

procedure TCnPanel.WMWindowPosChanging(var Message: TWMWindowPosChanging);
begin
  Invalidate;
  inherited;
end;

procedure TCnPanel.WMEraseBkgnd(var Message: TMessage);
begin
  if FTransparent then
    Message.Result := 0
  else
    inherited;
end;

procedure TCnPanel.Resize;
begin
  Invalidate;
  inherited;
end;

procedure TCnPanel.CMCtl3DChanged(var Message: TMessage);
begin
  inherited;
  RecreateWnd{$IFDEF FPC}(Self){$ENDIF};
end;

procedure TCnPanel.SetTransparent(const Value: Boolean);
var
  ES: Cardinal;
  I: Integer;
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    if csDesigning in ComponentState then
      Exit;

    if FTransparent and HandleAllocated then
    begin
      DoTransparentChanging;
      ES := GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_TRANSPARENT;
      SetWindowLong(Handle, GWL_EXSTYLE, ES);

      if Parent <> nil then
      try
        Parent.Invalidate;
      except
        ;
      end;

      for I := 0 to ControlCount - 1 do
      begin
        try
          Controls[I].Invalidate;
        except
          ;
        end;
      end;
    end
    else if HandleAllocated then
    begin
      DoTransparentChanging;
      ES := GetWindowLong(Handle, GWL_EXSTYLE) and not WS_EX_TRANSPARENT;
      SetWindowLong(Handle, GWL_EXSTYLE, ES);
    end;  
    Invalidate;
    DoTransparentChanged;
  end;
end;

procedure TCnPanel.DoTransparentChanged;
begin
  if Assigned(FTransparentChanged) then
    FTransParentChanged(Self);
end;

procedure TCnPanel.DoTransparentChanging;
begin
  if Assigned(FTransparentChanging) then
    FTransParentChanging(Self);
end;

end.
 
