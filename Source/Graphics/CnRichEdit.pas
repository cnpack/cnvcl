{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2025 CnPack 开发组                       }
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
{            网站地址：https://www.cnpack.org                                  }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnRichEdit;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：用隐藏的 RichEdit 控件渲染富文本并生成位图的解析单元
* 单元作者：CnPack 开发组
* 备    注：语法支持不完整，譬如没有表格，不支持嵌套列表等
* 开发平台：PWin7 + Delphi 5
* 兼容测试：PWin7 + Delphi 2009 ~
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2025.03.12 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, RichEdit;

type
  TCnRichEditRender = class
  private
    FRichEdit: TRichEdit;
    FBackBuffer: TBitmap;
    FTempParent: TWinControl;
    FBackgroundColor: TColor;
    procedure CreateHiddenRichEdit(FixedWidth: Integer = 0);
    procedure RenderToBackBuffer(RtfContent: TStream; FixedWidth: Integer = 0);
  public
    constructor Create;
    destructor Destroy; override;

    function RenderRtfToBitmap(RtfContent: TStream; FixedWidth: Integer = 0): TBitmap;
    {* 传入富文本流和指定宽度，计算布局尺寸并渲染到位图上，返回位图对象，需外部释放}
    procedure GetRtfSize(RtfContent: TStream; out Width, Height: Integer;
      FixedWidth: Integer = 0);
    {* 传入富文本流和指定宽度，计算布局尺寸，不进行实际渲染}

    property BackgroundColor: TColor read FBackgroundColor write FBackgroundColor;
    {* 背景色，默认白色}
  end;

implementation

type
  TCnRenderParentForm = class(TCustomForm)
  private
    FReqWidth: Integer;
    FReqHeight: Integer;
    procedure WMNotify(var Msg: TWMNotify); message WM_NOTIFY;
  end;

{ TCnRichEditRender }

constructor TCnRichEditRender.Create;
begin
  inherited;
  FBackBuffer := TBitmap.Create;
  FBackBuffer.PixelFormat := pf32bit;
  FBackgroundColor := clWhite;

  // 创建临时父容器（不可见窗体）
  FTempParent := TCnRenderParentForm.CreateNew(nil); // CreateNew 不加载 dfm
  FTempParent.Width := 0;
  FTempParent.Height := 0;
  FTempParent.HandleNeeded;
  FTempParent.Visible := False;
end;

destructor TCnRichEditRender.Destroy;
begin
  FreeAndNil(FBackBuffer);
  FreeAndNil(FRichEdit);
  FreeAndNil(FTempParent); // 释放临时父容器
  inherited;
end;

procedure TCnRichEditRender.CreateHiddenRichEdit(FixedWidth: Integer);
begin
  // 高版本 Delphi 里，需要 FreeAndNil(FRichEdit); 否则 FixedWidth 缩小时不生效
  if FRichEdit <> nil then
  begin
    if FixedWidth > 0 then
      FRichEdit.Width := FixedWidth
    else
      FRichEdit.Width := 1440;

    FRichEdit.Color := FBackgroundColor;
{$IFDEF UNICODE}
    // D2009 或以上版本，Width 改变后宽度不会更新，需要这里补一下
    FRichEdit.Perform(CM_RECREATEWND, 0, 0);
{$ENDIF}
  end
  else
  begin
    FRichEdit := TRichEdit.Create(FTempParent);
    if FixedWidth > 0 then
      FRichEdit.Width := FixedWidth
    else
      FRichEdit.Width := 1440;

    FRichEdit.Visible := False;
    FRichEdit.Parent := FTempParent;
    FRichEdit.ScrollBars := ssHorizontal;
    FRichEdit.WordWrap := False;
    FRichEdit.BorderStyle := bsNone;
    FRichEdit.Color := FBackgroundColor;
  end;
end;

procedure TCnRichEditRender.RenderToBackBuffer(RtfContent: TStream; FixedWidth: Integer);
var
  FormatRange: TFormatRange;
  W, H, LogPixX, LogPixY: Integer;
begin
  GetRtfSize(RtfContent, W, H, FixedWidth);
  LogPixX := GetDeviceCaps(FBackBuffer.Canvas.Handle, LOGPIXELSX);
  LogPixY := GetDeviceCaps(FBackBuffer.Canvas.Handle, LOGPIXELSY);

  FBackBuffer.Width := W;
  FBackBuffer.Height := H;
  FBackBuffer.Canvas.Brush.Color := FBackgroundColor;
  FBackBuffer.Canvas.FillRect(Rect(0, 0, FBackBuffer.Width, FBackBuffer.Height));

  // 高级渲染（处理复杂格式）
  ZeroMemory(@FormatRange, SizeOf(TFormatRange));

  FormatRange.hdc := FBackBuffer.Canvas.Handle;
  FormatRange.hdcTarget := FormatRange.hdc;
  FormatRange.rcPage := Rect(0, 0,
    MulDiv(FBackBuffer.Width, 1440, LogPixX),
    MulDiv(FBackBuffer.Height, 1440, LogPixY));
  FormatRange.rc := FormatRange.rcPage;
  FormatRange.chrg.cpMin := 0;
  FormatRange.chrg.cpMax := -1;

  // 执行格式化渲染
  FRichEdit.Perform(EM_FORMATRANGE, 1, LPARAM(@FormatRange));
  FRichEdit.Perform(EM_DISPLAYBAND, 0, LPARAM(@FormatRange.rcPage));
end;

function TCnRichEditRender.RenderRtfToBitmap(RtfContent: TStream; FixedWidth: Integer): TBitmap;
begin;
  try
    RenderToBackBuffer(RtfContent, FixedWidth);
    Result := TBitmap.Create;
    Result.Assign(FBackBuffer);
  except
    on E: Exception do
      raise Exception.Create('Render failed: ' + E.Message);
  end;
end;

procedure TCnRichEditRender.GetRtfSize(RtfContent: TStream; out Width,
  Height: Integer; FixedWidth: Integer);
var
  LogPixX, LogPixY, PhysicalWidth, PhysicalHeight: Integer;
begin
  // 创建控件时强制应用尺寸
  CreateHiddenRichEdit(FixedWidth); // 确保此处传入 FixedWidth

  // 以下 D5 ~ D7 下换行控制有效
  FRichEdit.Parent := FTempParent;
  Application.ProcessMessages; // 确保窗口句柄创建

  // 加载 RTF 内容
  if FixedWidth = 0 then
  begin
    FRichEdit.ScrollBars := ssHorizontal;
    FRichEdit.WordWrap := False;
    SendMessage(FRichEdit.Handle, EM_SETWORDBREAKPROC, 0, 0);
    FRichEdit.Perform(CM_RECREATEWND, 0, 0);
  end
  else
  begin
    FRichEdit.Width := FixedWidth;
    FRichEdit.ScrollBars := ssNone;
    FRichEdit.WordWrap := True;
  end;

  FRichEdit.BorderStyle := bsNone;
  FRichEdit.PlainText := False;
  FRichEdit.Clear;
  FRichEdit.Lines.LoadFromStream(RtfContent);

  // 自动计算所需尺寸
  FRichEdit.Perform(EM_REQUESTRESIZE, 0, 0);

  // 从临时父窗口获取计算后的尺寸
  PhysicalWidth := TCnRenderParentForm(FTempParent).FReqWidth;
  PhysicalHeight := TCnRenderParentForm(FTempParent).FReqHeight;

  LogPixX := GetDeviceCaps(FBackBuffer.Canvas.Handle, LOGPIXELSX);
  LogPixY := GetDeviceCaps(FBackBuffer.Canvas.Handle, LOGPIXELSY);

  // 准备位图缓冲区
  Width := MulDiv(PhysicalWidth, LogPixX, 96) + 4;
  Height := MulDiv(PhysicalHeight, LogPixY, 96) + 4;
end;

{ TCnRenderParentForm }

procedure TCnRenderParentForm.WMNotify(var Msg: TWMNotify);
var
  ReqResize: PReqSize;
begin
  if (Msg.NMHdr^.code = EN_REQUESTRESIZE) then
  begin
    ReqResize := PReqSize(Msg.NMHdr);
    FReqWidth := ReqResize^.rc.right - ReqResize^.rc.left;
    FReqHeight := ReqResize^.rc.bottom - ReqResize^.rc.top;
  end;
  inherited;
end;

end.
