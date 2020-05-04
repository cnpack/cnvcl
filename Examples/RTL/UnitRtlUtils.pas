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
    procedure btnGetMyModuleClick(Sender: TObject);
    procedure btnGetStackClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnHookIATClick(Sender: TObject);
    procedure btnUnHookIATClick(Sender: TObject);
    procedure btnCallMessageBoxClick(Sender: TObject);
  private
    procedure Proc1;
    procedure Proc2;
    procedure Proc3;
  public
    { Public declarations }
  end;

var
  FormRtlUtils: TFormRtlUtils;

implementation

//uses
//  JclDebug;

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
//  SL: TJclStackInfoList;
begin
  List := TCnStackInfoList.Create(True);
  mmoStack.Lines.Clear;
  List.DumpToStrings(mmoStack.Lines);
  List.Free;

//  SL := TJclStackInfoList.Create(False, Cardinal(-1), nil, False, nil, nil);
//  SL.AddToStrings(mmoMyModules.Lines, True, True, True, False);
//  SL.Free;
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
    if CnUnHookImportAddressTable(user32, 'MessageBoxA', OldMessageBoxA) then
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

end.
