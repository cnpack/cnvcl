{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2011 CnPack 开发组                       }
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

unit CnPanel;
{* |<PRE>
================================================================================
* 软件名称：界面控件包
* 单元名称：实现了透明效果的 CnPanel
* 单元作者：刘啸 (liuxiao@cnpack.org)
* 备    注：该单元当前仅为内部参考测试用
* 开发平台：PWin98SE + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id: CnImage.pas 138 2009-07-14 03:23:28Z zhoujingyu $
* 修改记录：2009.07.18 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Windows, Controls, Messages, ExtCtrls, Graphics;

type
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
    {* 是否透明，透明时会显示其下的内容}
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

  ParentCtl3d := False;
  Ctl3D := False;
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
  if FTransparent and not (csDesigning in ComponentState) then // 设计期不显示透明效果
  begin
    R := GetClientRect();
    FBuffer.Width := Width;
    FBuffer.Height := Height;
    FBuffer.Canvas.Brush.Style := bsSolid;
    FBuffer.Canvas.Brush.Color := Color;
    FBuffer.Canvas.FillRect(R);

    StretchBlt(FBuffer.Canvas.Handle, 0, 0, Width, Height,
               Canvas.Handle, 0, 0, Width, Height, cmSrcCopy);

    if Ctl3D then DrawEdge(FBuffer.Canvas.Handle, R, BDR_RAISEDINNER, BF_RECT);
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
      Flags := DrawTextBiDiModeFlags(Flags);
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
  RecreateWnd;
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
 