(* This unit is created by PODO generator *)

unit PODO_WARNINGS;

{$M+}

interface

uses
  Classes, SysUtils, CnDHibernateBase;

type
  TWarnings = class(TCnDHibernateBase)
  private
    FSelfId : String;
    FQQCode : String;
    FWarnTime : TDateTime;
    FReason : string;
    FUserName_FORMULA: string;
    FNameCard_FORMULA: string;
    function GetUserName_SQL: string;
    function GetNameCard_SQL: string;
  published
    property SelfId : String read FSelfId write FSelfId;
    property QQCode : String read FQQCode write FQQCode;
    property WarnTime : TDateTime read FWarnTime write FWarnTime;
    property Reason : string read FReason write FReason;
    property UserName_FORMULA: string read FUserName_FORMULA write FUserName_FORMULA;
    property UserName_SQL : string read GetUserName_SQL;
    property NameCard_FORMULA: string read FNameCard_FORMULA write FNameCard_FORMULA;
    property NameCard_SQL : string read GetNameCard_SQL;
  end;

implementation

{ TWarnings }

function TWarnings.GetNameCard_SQL: string;
begin
  Result := Format('select [NameCard] from [Members] where [QQCode]=''%s''',[QQCode]);
end;

function TWarnings.GetUserName_SQL: string;
begin
  Result := Format('select [UserName] from [Members] where [QQCode]=''%s''', [QQCode]);
end;

initialization
  RegisterClass(TWarnings);

end.