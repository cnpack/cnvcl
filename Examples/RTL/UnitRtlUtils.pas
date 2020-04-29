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
    procedure btnGetMyModuleClick(Sender: TObject);
    procedure btnGetStackClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormRtlUtils: TFormRtlUtils;

implementation

{$R *.DFM}

procedure TFormRtlUtils.btnGetMyModuleClick(Sender: TObject);
var
  List: TCnModuleInfoList;
begin
  List := TCnModuleInfoList.Create;
  mmoMyModules.Lines.Clear;
  List.DumpToStrings(mmoMyModules.Lines);
  List.Free;
end;

function GetEBP32: Pointer;
asm
        MOV     EAX, EBP
end;

procedure TFormRtlUtils.btnGetStackClick(Sender: TObject);
var
  List: TCnStackInfoList;
begin
  List := TCnStackInfoList.Create(GetEBP32);
  mmoStack.Lines.Clear;
  List.DumpToStrings(mmoStack.Lines);
  List.Free;
end;

procedure TFormRtlUtils.FormCreate(Sender: TObject);
begin
  pgc1.ActivePageIndex := 0;
end;

end.
