unit w_PersistentClassSample;

interface

uses Classes;

type
  TTest = class(TComponent)
  private
    FintProp: Integer;
    FstrProp: string;
    procedure SetintProp(const Value: Integer);
    procedure SetstrProp(const Value: string);
  public
  published
    property intProp: Integer read FintProp write SetintProp;
    property strProp: string read FstrProp write SetstrProp;
  end;

implementation

procedure TTest.SetintProp(const Value: Integer);
begin
  FintProp := Value;

end;

procedure TTest.SetstrProp(const Value: string);
begin
  FstrProp := Value;
end;


end.
