unit UnitOrigProvider;

interface

uses
  SysUtils, Classes, Windows, Dialogs, UnitOrigIntf;

function GetProvider: IUnknown;

implementation

type
  TOriginalProvider = class(TInterfacedObject, IOriginalInterface)
  private
    FNotifier: IOriginalNotifier;
  public
    procedure TestMethod(Data: Cardinal);
    function TestFunction(const S: string): Boolean;
    procedure AddNotifier(Notifier: IOriginalNotifier);
  end;

var
  OriginalProvider: IOriginalInterface = nil;

{ TOriginalProvider }

procedure TOriginalProvider.AddNotifier(Notifier: IOriginalNotifier);
begin
  ShowMessage('Add Notifier.');
  FNotifier := Notifier;
end;

function TOriginalProvider.TestFunction(const S: string): Boolean;
begin
  ShowMessage('Test Function in Original Provider: ' + S);
  Result := True;
end;

procedure TOriginalProvider.TestMethod(Data: Cardinal);
begin
  ShowMessage('Test Method in Original Provider: ' + IntToStr(Data));
  if FNotifier <> nil then
    (FNotifier as IOriginalNotifier).OnNotify(Self); // 设置了类型检查，如果外部传入的不是 IOriginalNotifier，就会出错
end;

function GetProvider: IUnknown;
begin
  Result := OriginalProvider;
end;

initialization
  OriginalProvider := TOriginalProvider.Create;

finalization
  OriginalProvider := nil;

end.
