unit UnitOrigProvider;

interface

uses
  SysUtils, Classes, Windows, Dialogs, UnitOrigIntf;

function GetProvider: IUnknown;

implementation

type
  TOriginalProvider = class(TInterfacedObject, IOriginalInterface)
  public
    procedure TestMethod(Data: Cardinal);
    function TestFunction(const S: string): Boolean;
  end;

var
  OriginalProvider: IOriginalInterface = nil;

{ TOriginalProvider }

function TOriginalProvider.TestFunction(const S: string): Boolean;
begin
  ShowMessage('Test Function in Original Provider: ' + S);
  Result := True;
end;

procedure TOriginalProvider.TestMethod(Data: Cardinal);
begin
  ShowMessage('Test Method in Original Provider: ' + IntToStr(Data));
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
