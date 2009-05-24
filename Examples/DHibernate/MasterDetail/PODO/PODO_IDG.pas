(* This unit is created by PODO generator *)

unit PODO_IDG;

{$M+}

interface

uses
  Classes, SysUtils, CnDHibernateBase;

type
  TIdg = class(TCnDHibernateBase)
  private
    FCODE : String;
    FNAME : String;
  published
    property CODE : String read FCODE write FCODE;
    property NAME : String read FNAME write FNAME;
  end;

implementation

initialization
  RegisterClass(TIdg);

end.