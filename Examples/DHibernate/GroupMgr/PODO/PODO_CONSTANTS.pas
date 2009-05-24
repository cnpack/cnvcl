(* This unit is created by PODO generator *)

unit PODO_CONSTANTS;

{$M+}

interface

uses
  Classes, SysUtils, CnDHibernateBase;

type
  TConstants = class(TCnDHibernateBase)
  private
    FCode : String;
    FName : String;
  published
    property Code : String read FCode write FCode;
    property Name : String read FName write FName;
  end;

implementation

initialization
  RegisterClass(TConstants);

end.