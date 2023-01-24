(* This unit is created by PODO generator *)

unit PODO_DETAILTABLE;

{$M+}

interface

uses
  Classes, SysUtils, CnDHibernateBase;

type
  TDetailTable = class(TCnDHibernateBase)
  private
    FID : String;
    FMainID : String;
    FWORK : String;
  published
    property ID : String read FID write FID;
    property MainID : String read FMainID write FMainID;
    property WORK : String read FWORK write FWORK;
  end;

implementation

initialization
  RegisterClass(TDetailTable);

end.