unit UnitRtlUtils;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, CnRtlUtils;

type
  TFormRtlUtils = class(TForm)
    pgc1: TPageControl;
    tsModule: TTabSheet;
    grpModule: TGroupBox;
    btnGetMyModule: TButton;
    mmoMyModules: TMemo;
    tsStack: TTabSheet;
    grpStack: TGroupBox;
    btnGetStack: TButton;
    mmoStack: TMemo;
    tsIATHook: TTabSheet;
    grpIATHook: TGroupBox;
    btnHookIAT: TButton;
    btnUnHookIAT: TButton;
    btnCallMessageBox: TButton;
    btnJCLHookMessageBoxA: TButton;
    grpException: TGroupBox;
    btnHookException: TButton;
    btnUnHookException: TButton;
    btnRaiseException: TButton;
    btnRaiseOSException: TButton;
    btnManuallyGetStack: TButton;
    procedure btnGetMyModuleClick(Sender: TObject);
    procedure btnGetStackClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnHookIATClick(Sender: TObject);
    procedure btnUnHookIATClick(Sender: TObject);
    procedure btnCallMessageBoxClick(Sender: TObject);
    procedure btnJCLHookMessageBoxAClick(Sender: TObject);
    procedure btnHookExceptionClick(Sender: TObject);
    procedure btnUnHookExceptionClick(Sender: TObject);
    procedure btnRaiseExceptionClick(Sender: TObject);
    procedure btnRaiseOSExceptionClick(Sender: TObject);
    procedure btnManuallyGetStackClick(Sender: TObject);
  private
    procedure Proc1;
    procedure Proc2;
    procedure Proc3;
    procedure Func1;
    procedure Func2;
    procedure Func3;
  public
    procedure MyExceptionHandler(ExceptObj: Exception; ExceptAddr: Pointer;
      IsOSException: Boolean; StackList: TCnStackInfoList);
  end;

var
  FormRtlUtils: TFormRtlUtils;

implementation

{$IFDEF USE_JCL}
uses
  JclDebug, JclHookExcept, JclPeImage;
{$ENDIF}

{$R *.DFM}

procedure TFormRtlUtils.btnCallMessageBoxClick(Sender: TObject);
begin
  MessageBoxA(0, 'Test', 'Test', MB_OK);
end;

procedure TFormRtlUtils.btnGetMyModuleClick(Sender: TObject);
var
  List: TCnModuleInfoList;
begin
  List := TCnModuleInfoList.Create(True);
  mmoMyModules.Lines.Clear;
  List.DumpToStrings(mmoMyModules.Lines);
  List.Free;
end;

function GetEBP32: Pointer;
asm
        MOV     EAX, EBP
end;

procedure TFormRtlUtils.btnGetStackClick(Sender: TObject);
begin
  Proc1;
end;

procedure TFormRtlUtils.FormCreate(Sender: TObject);
begin
  pgc1.ActivePageIndex := 0;
end;

procedure TFormRtlUtils.Proc1;
begin
  Proc2;
end;

procedure TFormRtlUtils.Proc2;
begin
  Proc3;
end;

procedure TFormRtlUtils.Proc3;
var
  List: TCnStackInfoList;
{$IFDEF USE_JCL}
  SL: TJclStackInfoList;
{$ENDIF}
begin
  List := TCnCurrentStackInfoList.Create(False);
  mmoStack.Lines.Clear;
  List.DumpToStrings(mmoStack.Lines);
  List.Free;

{$IFDEF USE_JCL}
  SL := TJclStackInfoList.Create(False, Cardinal(-1), nil, False, nil, nil);
  SL.AddToStrings(mmoMyModules.Lines, True, True, True, False);
  SL.Free;
{$ENDIF}
end;

type
  TMessageBoxA = function (hWnd: HWND; lpText, lpCaption: PAnsiChar; uType: UINT): Integer; stdcall;

var
  MessageBoxHooked: Boolean = False;
  OldMessageBoxA: Pointer = nil;

function MyMessageBoxA(hWnd: HWND; lpText, lpCaption: PAnsiChar; uType: UINT): Integer; stdcall;
var
  Old: TMessageBoxA;
begin
  if OldMessageBoxA <> nil then
  begin
    Old := TMessageBoxA(OldMessageBoxA);
    Result := Old(hWnd, 'Hooked Text', 'Hooked Caption', uType);
  end
  else
    Result := 0;
end;

procedure TFormRtlUtils.btnHookIATClick(Sender: TObject);
begin
  if not MessageBoxHooked then
  begin
    if CnHookImportAddressTable(user32, 'MessageBoxA', OldMessageBoxA, @MyMessageBoxA) then
    begin
      MessageBoxHooked := True;
      ShowMessage('Hook OK.');
    end
    else
      ShowMessage('Hook Fail.');
  end
  else
    ShowMessage('Already Hooked. Do Nothing.');
end;

procedure TFormRtlUtils.btnUnHookIATClick(Sender: TObject);
begin
  if MessageBoxHooked then
  begin
    if CnUnHookImportAddressTable(user32, 'MessageBoxA', OldMessageBoxA, @MyMessageBoxA) then
    begin
      MessageBoxHooked := False;
      ShowMessage('UnHook OK');
    end
    else
      ShowMessage('UnHook Fail.');
  end
  else
    ShowMessage('NOT Hooked. Please Hook it First.');
end;

procedure TFormRtlUtils.btnJCLHookMessageBoxAClick(Sender: TObject);
begin
{$IFDEF USE_JCL}
  OldMessageBoxA := GetProcAddress(GetModuleHandle(user32), 'MessageBoxA');
  if TJclPeMapImgHooks.ReplaceImport(TJclPeMapImgHooks.SystemBase, user32,
    OldMessageBoxA, @MyMessageBoxA) then
    ShowMessage('JCL Hooked.');
{$ENDIF}
end;

procedure TFormRtlUtils.btnHookExceptionClick(Sender: TObject);
begin
  CnHookException;
  CnSetAdditionalExceptionRecorder(MyExceptionHandler);
end;

procedure TFormRtlUtils.btnUnHookExceptionClick(Sender: TObject);
begin
  CnUnHookException;
end;

procedure TFormRtlUtils.btnRaiseExceptionClick(Sender: TObject);
begin
  raise Exception.Create('test');
end;

procedure TFormRtlUtils.MyExceptionHandler(ExceptObj: Exception;
  ExceptAddr: Pointer; IsOSException: Boolean; StackList: TCnStackInfoList);
begin
  ShowMessage(ExceptObj.Message);
  if ExceptObj is EExternal then
    ShowMessage('Is EExternal');

  if StackList <> nil then
  begin
    mmoStack.Lines.Clear;
    StackList.DumpToStrings(mmoStack.Lines);
  end;
end;

procedure TFormRtlUtils.btnRaiseOSExceptionClick(Sender: TObject);
var
  I, J: Integer;
begin
  I := 0;
  J := 3;
  if J / I = 1 then
    Caption := '';
end;

procedure TFormRtlUtils.btnManuallyGetStackClick(Sender: TObject);
begin
  Func1;
end;

procedure TFormRtlUtils.Func1;
begin
  Func2;
end;

procedure TFormRtlUtils.Func2;
begin
  Func3;
end;

procedure TFormRtlUtils.Func3;
var
  List: TCnManualStackInfoList;
{$IFDEF USE_JCL}
  SL: TJclStackInfoList;
{$ENDIF}
begin
  List := TCnManualStackInfoList.Create(GetEBP32, nil);
  mmoStack.Lines.Clear;
  List.DumpToStrings(mmoStack.Lines);
  List.Free;

{$IFDEF USE_JCL}
  SL := TJclStackInfoList.Create(False, Cardinal(-1), nil, False, nil, nil);
  SL.AddToStrings(mmoMyModules.Lines, True, True, True, False);
  SL.Free;
{$ENDIF}
end;

end.
