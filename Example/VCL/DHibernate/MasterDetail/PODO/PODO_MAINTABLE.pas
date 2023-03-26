(* This unit is created by PODO generator *)

unit PODO_MAINTABLE;

{$M+}

interface

uses
  Classes, SysUtils, CnDHibernateBase;

type
  TMainTable = class(TCnDHibernateBase)
  private
    FID : String;
    FNAME : String;
  published
    property ID : String read FID write FID;
    property NAME : String read FNAME write FNAME;
  end;

implementation

initialization
  RegisterClass(TMainTable);

end.