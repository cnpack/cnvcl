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

unit UnitOutput;
{* |<PRE>
================================================================================
* 软件名称：CnDebug 测试程序
* 单元名称：CnDebug 测试程序主单元
* 单元作者：刘啸(LiuXiao) liuxiao@cnpack.org
* 备    注：
* 开发平台：PWin2000 + Delphi 5
* 兼容测试：暂无（PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6）
* 本 地 化：该窗体中的字符串暂不符合本地化处理方式
* 单元标识：$Id: UnitOutput.pas,v 1.16 2009/01/02 08:27:38 liuxiao Exp $
* 修改记录：2005.02.01 V1.0
*               创建单元，实现部分移植功能
================================================================================
|</PRE>}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, ActnList, UnitThread;

type
  TForm1 = class(TForm)
    Button1: TButton;
    cbbLevel: TComboBox;
    chkLevel: TCheckBox;
    chkType: TCheckBox;
    cbbType: TComboBox;
    chkTag: TCheckBox;
    cbbTag: TComboBox;
    lblMsg: TLabel;
    edtMsg: TEdit;
    lblCount: TLabel;
    edtCount: TEdit;
    udCount: TUpDown;
    Bevel1: TBevel;
    Button2: TButton;
    rgMethod: TRadioGroup;
    btnEnter: TButton;
    btnLeave: TButton;
    Button3: TButton;
    Bevel2: TBevel;
    edtInt: TEdit;
    udInt: TUpDown;
    btnSendInt: TButton;
    edtFloat: TEdit;
    btnSendFloat: TButton;
    btnSendColor: TButton;
    btnSendBool: TButton;
    btnSendPoint: TButton;
    btnSendRect: TButton;
    pnlColor: TPanel;
    dlgColor: TColorDialog;
    btnDump: TButton;
    btnExcept: TButton;
    btnWriteComp: TButton;
    btnWriteObj: TButton;
    btnWriteCol: TButton;
    StatusBar1: TStatusBar;
    btnThread: TButton;
    tmr1: TTimer;
    Button4: TButton;
    btnEvaluate: TButton;
    btnEvaColl: TButton;
    btnDatetime: TButton;
    btnFmtError: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnEnterClick(Sender: TObject);
    procedure btnLeaveClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure btnSendIntClick(Sender: TObject);
    procedure btnSendFloatClick(Sender: TObject);
    procedure pnlColorClick(Sender: TObject);
    procedure btnSendColorClick(Sender: TObject);
    procedure btnSendBoolClick(Sender: TObject);
    procedure btnSendPointClick(Sender: TObject);
    procedure btnSendRectClick(Sender: TObject);
    procedure btnDumpClick(Sender: TObject);
    procedure btnExceptClick(Sender: TObject);
    procedure btnWriteCompClick(Sender: TObject);
    procedure btnWriteObjClick(Sender: TObject);
    procedure btnWriteColClick(Sender: TObject);
    procedure btnThreadClick(Sender: TObject);
    procedure tmr1Timer(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure btnEvaluateClick(Sender: TObject);
    procedure btnEvaCollClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edtMsgKeyPress(Sender: TObject; var Key: Char);
    procedure btnDatetimeClick(Sender: TObject);
    procedure btnFmtErrorClick(Sender: TObject);
  private
    { Private declarations }
    FTimeStamp: Boolean;
    FThread: TSendThread;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses CnDebug;

{$R *.dfm}

var
  I: Integer = 0;

procedure TForm1.Button1Click(Sender: TObject);
var
  AType: TCnMsgType;
  ALevel: Integer;
  ATag: string;
  I: Integer;
begin
  if chkLevel.Checked then ALevel := cbbLevel.ItemIndex else ALevel := CurrentLevel;
  if chkType.Checked then AType := TCnMsgType(cbbType.ItemIndex) else AType := cmtInformation;
  if chkTag.Checked then ATag := cbbTag.Text else ATag := '';
  if rgMethod.ItemIndex = 1 then
    for I := 1 to udCount.Position do
    begin
      Sleep(0);
      CnDebugger.TraceFull(edtMsg.Text, ATag, ALevel, AType);
    end
  else
    for I := 1 to udCount.Position do
    begin
      Sleep(0);
      CnDebugger.LogFull(edtMsg.Text, ATag, ALevel, AType);
    end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if not FTimeStamp then
  begin
    CnDebugger.StartTimeMark('1');
    Button2.Caption := '计时结束';
  end
  else
  begin
    CnDebugger.StopTimeMark('1');
    Button2.Caption := '计时开始';
  end;
  FTimeStamp := not FTimeStamp;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  cbbLevel.ItemIndex := 3;
  cbbType.ItemIndex := 0;
  CnDebugger.UseAppend := True;
end;

procedure TForm1.btnEnterClick(Sender: TObject);
begin
  CnDebugger.TraceEnter('A ProcName');
end;

procedure TForm1.btnLeaveClick(Sender: TObject);
begin
  CnDebugger.TraceLeave('A ProcName');
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  CnDebugger.TraceSeparator;
end;

procedure TForm1.btnSendIntClick(Sender: TObject);
begin
  if rgMethod.ItemIndex = 1 then
    CnDebugger.TraceInteger(udInt.Position)
  else
    CnDebugger.LogInteger(udInt.Position);
end;

procedure TForm1.btnSendFloatClick(Sender: TObject);
begin
  if rgMethod.ItemIndex = 1 then
    CnDebugger.TraceFloat(StrToFloat(edtFloat.Text))
  else
    CnDebugger.LogFloat(StrToFloat(edtFloat.Text));
end;

procedure TForm1.pnlColorClick(Sender: TObject);
begin
  if dlgColor.Execute then
    pnlColor.Color := dlgColor.Color;
end;

procedure TForm1.btnSendColorClick(Sender: TObject);
begin
  if rgMethod.ItemIndex = 1 then
    CnDebugger.TraceColor(pnlColor.Color)
  else
    CnDebugger.LogColor(pnlColor.Color);
end;

procedure TForm1.btnSendBoolClick(Sender: TObject);
begin
  if rgMethod.ItemIndex = 1 then
    CnDebugger.TraceBoolean(True)
  else
    CnDebugger.LogBoolean(False);
end;

procedure TForm1.btnSendPointClick(Sender: TObject);
begin
  if rgMethod.ItemIndex = 1 then
    CnDebugger.TracePoint(Mouse.CursorPos)
  else
    CnDebugger.LogPoint(Mouse.CursorPos);
end;

procedure TForm1.btnSendRectClick(Sender: TObject);
begin
  if rgMethod.ItemIndex = 1 then
    CnDebugger.TraceRect(ClientRect)
  else
    CnDebugger.LogRect(ClientRect);
end;

procedure TForm1.btnDumpClick(Sender: TObject);
var
  xx: array[0..255] of char;
begin
  xx[0]:='y';
  xx[1]:='x';
  if rgMethod.ItemIndex = 1 then
    CnDebugger.TraceMemDump(@xx, 256)
  else
    CnDebugger.LogMemDump(@xx, 256);
end;

procedure TForm1.btnExceptClick(Sender: TObject);
begin
{$IFNDEF USE_JCL}
  Application.MessageBox('无 JCL 参与编译，无法捕捉异常。', '提示', MB_OK + 
    MB_ICONWARNING);
{$ENDIF}
  raise Exception.Create('Test Exception.');
end;

procedure TForm1.btnWriteCompClick(Sender: TObject);
begin
  if rgMethod.ItemIndex = 1 then
    CnDebugger.TraceComponent(Self)
  else
    CnDebugger.LogComponent(Self);
end;

procedure TForm1.btnWriteObjClick(Sender: TObject);
begin
  if rgMethod.ItemIndex = 1 then
    CnDebugger.TraceObject(Self)
  else
    CnDebugger.LogObject(Self);
end;

procedure TForm1.btnWriteColClick(Sender: TObject);
begin
  if rgMethod.ItemIndex = 1 then
    CnDebugger.TraceCollection(Self.StatusBar1.Panels)
  else
    CnDebugger.LogCollection(Self.StatusBar1.Panels);
end;

procedure TForm1.btnThreadClick(Sender: TObject);
begin
  if FThread = nil then
  begin
    FThread := TSendThread.Create(True);
    FThread.FreeOnTerminate := True;
    FThread.Resume;
    btnThread.Caption := '停止发送';
  end
  else
  begin
    FThread.Terminate;
    FThread.WaitFor;
    FThread := nil;
    btnThread.Caption := '线程内发送';
  end;  
end;

procedure TForm1.tmr1Timer(Sender: TObject);
begin
  StatusBar1.SimpleText := Format('来消息数 %d, 发消息数 %d, 过滤数 %d',
    [CnDebugger.MessageCount, CnDebugger.PostedMessageCount, CnDebugger.DiscardedMessageCount]);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  OutputDebugString('Test OutputDebugString.');
end;

procedure TForm1.btnEvaluateClick(Sender: TObject);
begin
  CnDebugger.EvaluateObject(Self);
end;

procedure TForm1.btnEvaCollClick(Sender: TObject);
begin
  CnDebugger.EvaluateObject(Self.StatusBar1.Panels);
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if rgMethod.ItemIndex = 1 then
    CnDebugger.TraceVirtualKey(Key)
  else
    CnDebugger.LogVirtualKey(Key);
end;

procedure TForm1.edtMsgKeyPress(Sender: TObject; var Key: Char);
begin
  if rgMethod.ItemIndex = 1 then
    CnDebugger.TraceChar(Key)
  else
    CnDebugger.LogChar(Key);
end;

procedure TForm1.btnDatetimeClick(Sender: TObject);
begin
  if rgMethod.ItemIndex = 1 then
    CnDebugger.TraceDateTime(Date + Time)
  else
    CnDebugger.LogDateTime(Date + Time);
end;

procedure TForm1.btnFmtErrorClick(Sender: TObject);
begin
  if rgMethod.ItemIndex = 1 then
    CnDebugger.TraceFmt('A Sample Error Format String: %d', [Caption])
  else
    CnDebugger.LogFmt('A Sample Error Format String: %d', [Caption]);
end;

end.
