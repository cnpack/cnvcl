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

unit UnitOutput;
{* |<PRE>
================================================================================
* 软件名称：CnDebug 测试程序
* 单元名称：CnDebug 测试程序主单元
* 单元作者：CnPack 开发组 master@cnpack.org
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

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, UnitThread, Buttons,
  ImgList, Menus {$IFDEF TGRAPHIC_SUPPORT_PARTIALTRANSPARENCY}, PNGImage {$ENDIF};

type
  ITest = interface
    procedure Test(A: Integer);
  end;

  TTest = class(TInterfacedObject, ITest)
    procedure Test(A: Integer);
  end;

  TFormSend = class(TForm)
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
    btnEval: TBitBtn;
    btnImageList: TButton;
    ilTest: TImageList;
    btnStack: TButton;
    btnConstArray: TButton;
    btnClass: TButton;
    btnInterface: TButton;
    btnAddr: TButton;
    btnEBPAddr: TButton;
    pm1: TPopupMenu;
    Test11: TMenuItem;
    N1: TMenuItem;
    Test21: TMenuItem;
    Test211: TMenuItem;
    Test221: TMenuItem;
    btnEvaluateMenu: TButton;
    btnEvaluateBmp: TButton;
    btnSet: TButton;
    btnSize: TButton;
    btnAnsiCharSet: TButton;
    btnWideCharSet: TButton;
    btnWatchClear: TButton;
    btnEvaluateScreen: TButton;
    btnFindComponent: TButton;
    btnFindControl: TButton;
    btnEvaluateTransBmp: TButton;
    btnEvaluateImage: TButton;
    img1: TImage;
    dlgOpen1: TOpenDialog;
    btnDrawTransparent: TButton;
    btnEvaluateSample: TButton;
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
    procedure btnEvalClick(Sender: TObject);
    procedure btnImageListClick(Sender: TObject);
    procedure btnStackClick(Sender: TObject);
    procedure btnConstArrayClick(Sender: TObject);
    procedure btnClassClick(Sender: TObject);
    procedure btnInterfaceClick(Sender: TObject);
    procedure btnAddrClick(Sender: TObject);
    procedure btnEBPAddrClick(Sender: TObject);
    procedure btnEvaluateMenuClick(Sender: TObject);
    procedure btnEvaluateBmpClick(Sender: TObject);
    procedure btnSetClick(Sender: TObject);
    procedure btnSizeClick(Sender: TObject);
    procedure btnAnsiCharSetClick(Sender: TObject);
    procedure btnWideCharSetClick(Sender: TObject);
    procedure btnWatchClearClick(Sender: TObject);
    procedure btnEvaluateScreenClick(Sender: TObject);
    procedure btnFindComponentClick(Sender: TObject);
    procedure btnFindControlClick(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure btnEvaluateTransBmpClick(Sender: TObject);
    procedure btnEvaluateImageClick(Sender: TObject);
    procedure btnDrawTransparentClick(Sender: TObject);
    procedure btnEvaluateSampleClick(Sender: TObject);
  private
    FComponenet: TComponent;
    FTimeStamp: Boolean;
    FThread: TSendThread;
    FOldWndProc: TWndMethod;
    procedure NewWindowProc(var AMsg: TMessage);
    procedure DebuggerFindComponent(Sender: TObject; AComponent: TComponent;
      var Cancel: Boolean);
    procedure DebuggerFindControl(Sender: TObject; AControl: TControl;
      var Cancel: Boolean);
  public
    { Public declarations }
  end;

var
  FormSend: TFormSend;

implementation

uses
  CnDebug, CnSampleComponent;

{$R *.lfm}

var
  I: Integer = 0;

procedure TFormSend.Button1Click(Sender: TObject);
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

procedure TFormSend.Button2Click(Sender: TObject);
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

procedure TFormSend.FormCreate(Sender: TObject);
begin
  cbbLevel.ItemIndex := 3;
  cbbType.ItemIndex := 0;
  CnDebugger.UseAppend := True;
  Icon := Application.Icon;

  FOldWndProc := WindowProc;
  WindowProc := NewWindowProc;
end;

procedure TFormSend.btnEnterClick(Sender: TObject);
begin
  CnDebugger.TraceEnter('A ProcName');
end;

procedure TFormSend.btnLeaveClick(Sender: TObject);
begin
  CnDebugger.TraceLeave('A ProcName');
end;

procedure TFormSend.Button3Click(Sender: TObject);
begin
  CnDebugger.TraceSeparator;
end;

procedure TFormSend.btnSendIntClick(Sender: TObject);
begin
  if rgMethod.ItemIndex = 1 then
    CnDebugger.TraceInteger(udInt.Position)
  else
    CnDebugger.LogInteger(udInt.Position);
end;

procedure TFormSend.btnSendFloatClick(Sender: TObject);
begin
  if rgMethod.ItemIndex = 1 then
    CnDebugger.TraceFloat(StrToFloat(edtFloat.Text))
  else
    CnDebugger.LogFloat(StrToFloat(edtFloat.Text));
end;

procedure TFormSend.pnlColorClick(Sender: TObject);
begin
  if dlgColor.Execute then
    pnlColor.Color := dlgColor.Color;
end;

procedure TFormSend.btnSendColorClick(Sender: TObject);
begin
  if rgMethod.ItemIndex = 1 then
    CnDebugger.TraceColor(pnlColor.Color)
  else
    CnDebugger.LogColor(pnlColor.Color);
end;

procedure TFormSend.btnSendBoolClick(Sender: TObject);
begin
  if rgMethod.ItemIndex = 1 then
    CnDebugger.TraceBoolean(True)
  else
    CnDebugger.LogBoolean(False);
end;

procedure TFormSend.btnSendPointClick(Sender: TObject);
begin
  if rgMethod.ItemIndex = 1 then
    CnDebugger.TracePoint(Mouse.CursorPos)
  else
    CnDebugger.LogPoint(Mouse.CursorPos);
end;

procedure TFormSend.btnSendRectClick(Sender: TObject);
begin
  if rgMethod.ItemIndex = 1 then
    CnDebugger.TraceRect(ClientRect)
  else
    CnDebugger.LogRect(ClientRect);
end;

procedure TFormSend.btnDumpClick(Sender: TObject);
var
  xx: array[0..255] of char;
begin
  xx[0] := 'y';
  xx[1] := 'x';
  if rgMethod.ItemIndex = 1 then
    CnDebugger.TraceMemDump(@xx, 256)
  else
    CnDebugger.LogMemDump(@xx, 256);
end;

procedure TFormSend.btnExceptClick(Sender: TObject);
begin
  if rgMethod.ItemIndex = 1 then
    CnDebugger.TraceException(Exception.Create('Test Exception for output.'))
  else
    CnDebugger.LogException(Exception.Create('Test Exception for output.'));
{$IFNDEF CAPTURE_STACK}
  Application.MessageBox('无 CAPTURE_STACK 宏定义，无法捕捉异常。', '提示', MB_OK +
    MB_ICONWARNING);
{$ENDIF}
  raise Exception.Create('Test Exception.');
end;

procedure TFormSend.btnWriteCompClick(Sender: TObject);
begin
  if rgMethod.ItemIndex = 1 then
    CnDebugger.TraceComponent(Self)
  else
    CnDebugger.LogComponent(Self);
end;

procedure TFormSend.btnWriteObjClick(Sender: TObject);
begin
  if rgMethod.ItemIndex = 1 then
    CnDebugger.TraceObject(Self)
  else
    CnDebugger.LogObject(Self);
end;

procedure TFormSend.btnWriteColClick(Sender: TObject);
begin
  if rgMethod.ItemIndex = 1 then
    CnDebugger.TraceCollection(Self.StatusBar1.Panels)
  else
    CnDebugger.LogCollection(Self.StatusBar1.Panels);
end;

procedure TFormSend.btnThreadClick(Sender: TObject);
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
    CnDebugger.WatchClear('Count');
    btnThread.Caption := '线程内发送';
  end;  
end;

procedure TFormSend.tmr1Timer(Sender: TObject);
begin
  StatusBar1.SimpleText := Format('来消息数 %d, 发消息数 %d, 过滤数 %d',
    [CnDebugger.MessageCount, CnDebugger.PostedMessageCount, CnDebugger.DiscardedMessageCount]);
end;

procedure TFormSend.Button4Click(Sender: TObject);
begin
  OutputDebugString('Test OutputDebugString.');
end;

procedure TFormSend.btnEvaluateClick(Sender: TObject);
begin
  CnDebugger.EvaluateObject(Self);
end;

procedure TFormSend.btnEvaCollClick(Sender: TObject);
begin
  CnDebugger.EvaluateObject(Self.StatusBar1.Panels);
end;

procedure TFormSend.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if rgMethod.ItemIndex = 1 then
    CnDebugger.TraceVirtualKey(Key)
  else
    CnDebugger.LogVirtualKey(Key);
end;

procedure TFormSend.edtMsgKeyPress(Sender: TObject; var Key: Char);
begin
  if rgMethod.ItemIndex = 1 then
    CnDebugger.TraceChar(Key)
  else
    CnDebugger.LogChar(Key);
end;

procedure TFormSend.btnDatetimeClick(Sender: TObject);
begin
  if rgMethod.ItemIndex = 1 then
    CnDebugger.TraceDateTime(Date + Time)
  else
    CnDebugger.LogDateTime(Date + Time);
end;

procedure TFormSend.btnFmtErrorClick(Sender: TObject);
begin
  if rgMethod.ItemIndex = 1 then
    CnDebugger.TraceFmt('A Sample Error Format String: %d', [Caption])
  else
    CnDebugger.LogFmt('A Sample Error Format String: %d', [Caption]);
end;

procedure TFormSend.btnEvalClick(Sender: TObject);
begin
  CnDebugger.EvaluateObject(Sender);
end;

procedure TFormSend.btnImageListClick(Sender: TObject);
begin
  CnDebugger.EvaluateObject(ilTest);
end;

procedure TFormSend.btnStackClick(Sender: TObject);
begin
  if rgMethod.ItemIndex = 1 then
    CnDebugger.TraceCurrentStack('Trace Here')
  else
    CnDebugger.LogCurrentStack('Log Here');
end;

procedure TFormSend.btnConstArrayClick(Sender: TObject);
var
  a: Integer;
  b: Boolean;
  c: Char;
  d: Extended;
  e: ShortString;
  f: Pointer;
  g: PChar;
  h: TButton;
  i: TClass;
  j: WideChar;
  k: PWideChar;
  l: AnsiString;
  m: Currency;
  n: Variant;
  o: IUnknown;
  p: WideString;
  q: Int64;
  s: string; // Will be UnicodeString if under Unicode Env.
begin
  a := 10;
  b := True;
  c := 'a';
  d := 2;
  e := 'Sit';
  f := Pointer(Self);
  g := 'Q';
  h := TButton(Sender);
  i := TScreen;
  j := #20976;
  k := '吃';
  l := 'Abcs';
  m := 4.453;
  n := 5;
  o := nil;
  p := 'CnPack';
  q := 733;
  s := '睡觉';

  if rgMethod.ItemIndex = 1 then
    CnDebugger.TraceConstArray([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, s])
  else
    CnDebugger.LogConstArray([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, s]);
end;

procedure TFormSend.btnClassClick(Sender: TObject);
begin
  if rgMethod.ItemIndex = 1 then
    CnDebugger.TraceClass(TFormSend)
  else
    CnDebugger.LogClass(TFormSend);
end;

procedure TFormSend.btnInterfaceClick(Sender: TObject);
var
//  P: Pointer;
  IT: ITest;
begin
  IT := TTest.Create;
  if rgMethod.ItemIndex = 1 then
    CnDebugger.TraceInterface(IT)
  else
    CnDebugger.LogInterface(IT);
//  P := @IT.Test;  // Syntax Error
end;

procedure TFormSend.btnAddrClick(Sender: TObject);
var
  I, J: Integer;
begin
  try
    // raise Exception.Create('Test Address of Exception.');
    I := 0;
    J := 3;
    if J / I = 0 then
      Exit;
  except
    // 这段代码有问题：StackFromAddress 需要的是 EBP 地址才能跟踪堆栈，给 EIP 或 ExceptAddr 无法跟踪
    if rgMethod.ItemIndex = 1 then
      CnDebugger.TraceStackFromAddress(ExceptAddr)
    else
      CnDebugger.LogStackFromAddress(ExceptAddr);
  end;
end;

function GetEBP: Pointer;
asm
        MOV     EAX, EBP
end;

procedure TFormSend.btnEBPAddrClick(Sender: TObject);
begin
  if rgMethod.ItemIndex = 1 then
    CnDebugger.TraceStackFromAddress(GetEBP)
  else
    CnDebugger.LogStackFromAddress(GetEBP);
end;

procedure TFormSend.btnEvaluateMenuClick(Sender: TObject);
begin
  CnDebugger.EvaluateObject(pm1);
end;

procedure TFormSend.btnEvaluateScreenClick(Sender: TObject);
begin
  CnDebugger.EvaluateObject(Screen);
end;

procedure TFormSend.btnEvaluateBmpClick(Sender: TObject);
var
  Bmp: TBitmap;
  R: TRect;
begin
  Bmp := TBitmap.Create;
  Bmp.PixelFormat := pf24bit;
  Bmp.Width := 760;
  Bmp.Height := 260;

  Bmp.Canvas.Brush.Style := bsSolid;
  Bmp.Canvas.Brush.Color := clRed;
  R := Rect(0, 0, Bmp.Width div 2, Bmp.Height div 2);
  Bmp.Canvas.FillRect(R);

  Bmp.Canvas.Brush.Color := clNavy;
  R := Rect(Bmp.Width div 2, Bmp.Height div 2, Bmp.Width, Bmp.Height);
  Bmp.Canvas.FillRect(R);

  CnDebugger.EvaluateObject(Bmp, True);
  Bmp.Free;
end;

procedure TFormSend.btnSetClick(Sender: TObject);
begin
  if rgMethod.ItemIndex = 1 then
    CnDebugger.TraceSet(Anchors, SizeOf(Anchors), TypeInfo(TAnchorKind))
  else
    CnDebugger.LogSet(Anchors, SizeOf(Anchors), TypeInfo(TAnchorKind));
end;

procedure TFormSend.btnSizeClick(Sender: TObject);
var
  Size: TSize;
begin
  Size := Canvas.TextExtent('Test Me');
  if rgMethod.ItemIndex = 1 then
    CnDebugger.TraceSize(Size)
  else
    CnDebugger.LogSize(Size);
end;

procedure TFormSend.btnAnsiCharSetClick(Sender: TObject);
var
  CS: TCnAnsiCharSet;
begin
  CS := ['A'..'Z'];
  if rgMethod.ItemIndex = 1 then
    CnDebugger.TraceAnsiCharSet(CS)
  else
    CnDebugger.LogAnsiCharSet(CS);
end;

procedure TFormSend.btnWideCharSetClick(Sender: TObject);
{$IFDEF UNICODE}
var
  CS: TCnWideCharSet;
{$ENDIF}
begin
{$IFDEF UNICODE}
  CS := ['吃','饭'];
  if rgMethod.ItemIndex = 1 then
    CnDebugger.TraceWideCharSet(CS)
  else
    CnDebugger.LogWideCharSet(CS);
{$ELSE}
  ShowMessage('Unicode Currently NOT Supported.');
{$ENDIF}
end;

procedure TFormSend.btnWatchClearClick(Sender: TObject);
begin
  CnDebugger.WatchClear('Count');
end;

{ TTest }

procedure TTest.Test(A: Integer);
begin

end;

procedure TFormSend.NewWindowProc(var AMsg: TMessage);
begin
  CnDebugger.LogWindowMessage(AMsg.Msg);
  FOldWndProc(AMsg);
end;

procedure TFormSend.btnFindComponentClick(Sender: TObject);
begin
  CnDebugger.OnFindComponent := DebuggerFindComponent;
  CnDebugger.FindComponent;
end;

procedure TFormSend.btnFindControlClick(Sender: TObject);
begin
  CnDebugger.OnFindControl := DebuggerFindControl;
  CnDebugger.FindControl;
end;

procedure TFormSend.DebuggerFindComponent(Sender: TObject;
  AComponent: TComponent; var Cancel: Boolean);
begin
  (Sender as TCnDebugger).LogMsg('On Find Component: ' + AComponent.ClassName + ' - ' + AComponent.Name);
end;

procedure TFormSend.DebuggerFindControl(Sender: TObject;
  AControl: TControl; var Cancel: Boolean);
begin
  (Sender as TCnDebugger).LogMsg('On Find Control: ' + AControl.ClassName + ' - ' + AControl.Name);
end;

procedure TFormSend.FormClick(Sender: TObject);
begin
{$IFDEF SUPPORT_ENHANCED_RTTI}
  if rgMethod.ItemIndex = 1 then
    CnDebugger.TraceEnumType<TAnchorKind>
  else
    CnDebugger.LogEnumType<TAnchorKind>;
{$ENDIF}
end;

procedure TFormSend.btnEvaluateTransBmpClick(Sender: TObject);
{$IFDEF TGRAPHIC_SUPPORT_PARTIALTRANSPARENCY}
var
  Bmp: TBitmap;
  Png: TPngImage;
{$ENDIF}
begin
{$IFDEF TGRAPHIC_SUPPORT_PARTIALTRANSPARENCY}
  ShowMessage('Please Open a Transparent PNG File.');
  if dlgOpen1.Execute then
  begin
    Bmp := TBitmap.Create;
    Png := TPngImage.Create;

    Png.LoadFromFile(dlgOpen1.FileName);
    Bmp.Assign(Png);

    CnDebugger.EvaluateObject(Bmp, True);
    Bmp.Free;
    Png.Free;
  end;
{$ELSE}
  ShowMessage('Please RUN under Delphi 2009 or Above.');
{$ENDIF}
end;

procedure TFormSend.btnEvaluateImageClick(Sender: TObject);
begin
  CnDebugger.EvaluateObject(img1.Picture);
end;

procedure TFormSend.btnDrawTransparentClick(Sender: TObject);
var
  Bmp: TBitmap;
  Y, SW: Integer;
  R: TRect;
begin
  Bmp := TBitmap.Create;
  Bmp.PixelFormat := pf32Bit;
{$IFDEF TGRAPHIC_SUPPORT_PARTIALTRANSPARENCY}
  Bmp.AlphaFormat := afDefined;
{$ENDIF}
  Bmp.Transparent := True;      // 3 Very Important !

  Bmp.Width := 400;
  Bmp.Height := 300;

  // Draw transparent background
  SW := Bmp.Width * SizeOf(TRGBQuad);
  for Y := 0 to Bmp.Height - 1 do
    FillChar(Bmp.ScanLine[Y]^, SW, 0);

  Bmp.Canvas.Brush.Color := clGreen;
  Bmp.Canvas.Brush.Style := bsSolid;

  R := Rect(Bmp.Width div 3, Bmp.Height div 3, 2 * Bmp.Width div 3, 2 * Bmp.Height div 3);
  Bmp.Canvas.FillRect(R);

  Bmp.Canvas.Brush.Style := bsClear;
  Bmp.Canvas.Font.Size := 36;
  Bmp.Canvas.Font.Style := [fsBold];
  Bmp.Canvas.Font.Color := clRed;
  Bmp.Canvas.TextOut(Bmp.Width div 2 - 60, Bmp.Height div 2 - 40, 'Hello!');

  CnDebugger.EvaluateObject(Bmp, True);
  // Bmp.SaveToFile('C:\CnPack\A64.bmp'); 全黑

  Bmp.Free;
//{$ELSE}
//  ShowMessage('Please RUN under Delphi 2009 or Above.');
//{$ENDIF}
end;

procedure TFormSend.btnEvaluateSampleClick(Sender: TObject);
begin
  if FComponenet = nil then
    FComponenet := TCnSampleComponent.Create(Self);

  (FComponenet as TCnSampleComponent).OnClick := btnEvaluateSampleClick;
  CnDebugger.EvaluateObject(FComponenet);
end;

end.
