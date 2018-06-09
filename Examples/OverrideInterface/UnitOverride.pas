unit UnitOverride;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComObj, StdCtrls;

type
  IOverrideNotifier = interface
  // Same with IOriginalNotifier
    procedure OnNotify(Sender: TObject);
  end;
  
  // Same with IOriginalInterface except IOverrideNotifier
  IOverrideProvider = interface
    procedure TestMethod(Data: Cardinal);
    function TestFunction(const S: string): Boolean;
    procedure AddNotifier(Notifier: IOverrideNotifier);
  end;

  TOverrideNotifier = class(TInterfacedObject, IOverrideNotifier)
  public
    procedure OnNotify(Sender: TObject);
  end;

  TFormTest = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FNotifier: IOverrideNotifier;
  public
    { Public declarations }
  end;

var
  FormTest: TFormTest;

implementation

uses
  UnitOrigProvider;

{$R *.DFM}

const
  GUID_ORIG_PROVIDER = '{EE690722-0845-4251-A48B-151526EDD74D}';
  GUID_ORIG_NOTIFIER = '{4E3A8311-1D36-4C92-8A05-D388642B0038}';

procedure TFormTest.FormCreate(Sender: TObject);
var
  Guid: TGUID;
  Unk: IUnknown;
  Provider: IOverrideProvider;
begin
  FNotifier := TOverrideNotifier.Create;

  Unk := GetProvider;
  Guid := StringToGUID(GUID_ORIG_PROVIDER);
  if Supports(Unk, Guid, Provider) then
  begin
    Provider.AddNotifier(FNotifier);
    Provider.TestMethod(0);
    Provider.TestFunction('Test');

    Provider := nil;
  end;
end;

{ TOverrideNotifier }

procedure TOverrideNotifier.OnNotify(Sender: TObject);
begin
  ShowMessage('TOverrideNotifier.OnNotify');
end;

procedure TFormTest.FormDestroy(Sender: TObject);
begin
  FNotifier := nil;
end;

end.
