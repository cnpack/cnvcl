(* This unit is created by PODO generator *)

unit PODO_RESEARCHS;

{$M+}

interface

uses
  Classes, SysUtils, CnDHibernateBase;

type
  TResearchs = class(TCnDHibernateBase)
  private
    FSelfId : String;
    FQQCode : String;
    FTime : TDateTime;
    FContext : string;
    FNameCard_FORMULA: string;
    FUserName_FORMULA: string;
    function GetNameCard_SQL: string;
    function GetUserName_SQL: string;
  published
    property SelfId : String read FSelfId write FSelfId;
    property QQCode : String read FQQCode write FQQCode;
    property Time : TDateTime read FTime write FTime;
    property Context : string read FContext write FContext;
    property UserName_FORMULA: string read FUserName_FORMULA write FUserName_FORMULA;
    property UserName_SQL : string read GetUserName_SQL;
    property NameCard_FORMULA: string read FNameCard_FORMULA write FNameCard_FORMULA;
    property NameCard_SQL : string read GetNameCard_SQL;
  end;

implementation

{ TResearchs }

function TResearchs.GetNameCard_SQL: string;
begin
  Result := Format('select [NameCard] from [Members] where [QQCode]=''%s''',[QQCode]);
end;

function TResearchs.GetUserName_SQL: string;
begin
  Result := Format('select [UserName] from [Members] where [QQCode]=''%s''', [QQCode]);
end;

initialization
  RegisterClass(TResearchs);

end.