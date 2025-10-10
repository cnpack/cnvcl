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

unit CnRichEdit;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ������ص� RichEdit �ؼ���Ⱦ���ı�������λͼ�Ľ�����Ԫ
* ��Ԫ���ߣ�CnPack ������
* ��    ע���﷨֧�ֲ�������Ʃ��û�б�񣬲�֧��Ƕ���б��
* ����ƽ̨��PWin7 + Delphi 5
* ���ݲ��ԣ�PWin7 + Delphi 2009 ~
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2025.03.12 V1.0
*               ������Ԫ
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
    {* ���븻�ı�����ָ����ȣ����㲼�ֳߴ粢��Ⱦ��λͼ�ϣ�����λͼ�������ⲿ�ͷš�
       ���ı���Ҫ���� AnsiString д�ɵģ������� UnicodeString}
    procedure GetRtfSize(RtfContent: TStream; out Width, Height: Integer;
      FixedWidth: Integer = 0);
    {* ���븻�ı�����ָ����ȣ����㲼�ֳߴ磬������ʵ����Ⱦ��
       ���ı���Ҫ���� AnsiString д�ɵģ������� UnicodeString}

    property BackgroundColor: TColor read FBackgroundColor write FBackgroundColor;
    {* ����ɫ��Ĭ�ϰ�ɫ}
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

  // ������ʱ�����������ɼ����壩
  FTempParent := TCnRenderParentForm.CreateNew(nil); // CreateNew ������ dfm
  FTempParent.Width := 0;
  FTempParent.Height := 0;
  FTempParent.HandleNeeded;
  FTempParent.Visible := False;
end;

destructor TCnRichEditRender.Destroy;
begin
  FreeAndNil(FBackBuffer);
  FreeAndNil(FRichEdit);
  FreeAndNil(FTempParent); // �ͷ���ʱ������
  inherited;
end;

procedure TCnRichEditRender.CreateHiddenRichEdit(FixedWidth: Integer);
begin
  // �߰汾 Delphi ���Ҫ FreeAndNil(FRichEdit); ���� FixedWidth ��Сʱ����Ч
  if FRichEdit <> nil then
  begin
    if FixedWidth > 0 then
      FRichEdit.Width := FixedWidth
    else
      FRichEdit.Width := 1440;

    FRichEdit.Color := FBackgroundColor;
{$IFDEF UNICODE}
    // D2009 �����ϰ汾��Width �ı���Ȳ�����£���Ҫ���ﲹһ��
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

  // �߼���Ⱦ�������Ӹ�ʽ��
  ZeroMemory(@FormatRange, SizeOf(TFormatRange));

  FormatRange.hdc := FBackBuffer.Canvas.Handle;
  FormatRange.hdcTarget := FormatRange.hdc;
  FormatRange.rcPage := Rect(0, 0,
    MulDiv(FBackBuffer.Width, 1440, LogPixX),
    MulDiv(FBackBuffer.Height, 1440, LogPixY));
  FormatRange.rc := FormatRange.rcPage;
  FormatRange.chrg.cpMin := 0;
  FormatRange.chrg.cpMax := -1;

  // ִ�и�ʽ����Ⱦ
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
  // �����ؼ�ʱǿ��Ӧ�óߴ�
  CreateHiddenRichEdit(FixedWidth); // ȷ���˴����� FixedWidth

  // ���� D5 ~ D7 �»��п�����Ч
  FRichEdit.Parent := FTempParent;
  Application.ProcessMessages; // ȷ�����ھ������

  // ���� RTF ����
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

  // �Զ���������ߴ�
  FRichEdit.Perform(EM_REQUESTRESIZE, 0, 0);

  // ����ʱ�����ڻ�ȡ�����ĳߴ�
  PhysicalWidth := TCnRenderParentForm(FTempParent).FReqWidth;
  PhysicalHeight := TCnRenderParentForm(FTempParent).FReqHeight;

  LogPixX := GetDeviceCaps(FBackBuffer.Canvas.Handle, LOGPIXELSX);
  LogPixY := GetDeviceCaps(FBackBuffer.Canvas.Handle, LOGPIXELSY);

  // ׼��λͼ������
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
