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
    procedure btnGetMyModuleClick(Sender: TObject);
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

end.
