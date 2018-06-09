unit UnitOverride;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComObj, StdCtrls;

type
  TFormTest = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  // Same with IOriginalInterface
  IOverrideProvider = interface
    procedure TestMethod(Data: Cardinal);
    function TestFunction(const S: string): Boolean;
  end;

var
  FormTest: TFormTest;

implementation

uses
  UnitOrigProvider;

{$R *.DFM}

procedure TFormTest.FormCreate(Sender: TObject);
var
  Guid: TGUID;
  Unk: IUnknown;
  Provider: IOverrideProvider;
begin
  Unk := GetProvider;
  Guid := StringToGUID('{EE690722-0845-4251-A48B-151526EDD74D}');
  if Supports(Unk, Guid, Provider) then
  begin
    Provider.TestMethod(0);
    Provider.TestFunction('Test');
    Provider := nil;
  end;
end;

end.
