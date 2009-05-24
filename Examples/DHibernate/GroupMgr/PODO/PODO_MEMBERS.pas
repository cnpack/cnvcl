(* This unit is created by PODO generator *)

unit PODO_MEMBERS;

{$M+}

interface

uses
  Classes, SysUtils, CnDHibernateBase;

type
  TMembers = class(TCnDHibernateBase)
  private
    FQQCode : WideString;
    FUserName : WideString;
    FSex : WideString;
    FAge : WideString;
    FArea : WideString;
    FNameCard : WideString;
    FEmail : WideString;
    FWebSite : WideString;
    FResearch : Variant;
    FStatus : WideString;
    FOutReason : Variant;
    FInTime : TDateTime;
    FOutTime : TDateTime;
    FIdentity : WideString;
    FSex_FORMULA: WideString;
    FStatus_FORMULA: WideString;
    FGotResearch: WideString;
    FGotWarn: WideString;
    function GetSex_SQL: WideString;
    function GetStatus_SQL: WideString;
  published
    property QQCode : WideString read FQQCode write FQQCode;
    property UserName : WideString read FUserName write FUserName;
    property Sex : WideString read FSex write FSex;
    property Age : WideString read FAge write FAge;
    property Area : WideString read FArea write FArea;
    property NameCard : WideString read FNameCard write FNameCard;
    property Email : WideString read FEmail write FEmail;
    property WebSite : WideString read FWebSite write FWebSite;
    property Research : Variant read FResearch write FResearch;
    property Status : WideString read FStatus write FStatus;
    property OutReason : Variant read FOutReason write FOutReason;
    property InTime : TDateTime read FInTime write FInTime;
    property OutTime : TDateTime read FOutTime write FOutTime;
    property Identity : WideString read FIdentity write FIdentity;
    property GotWarn: WideString read FGotWarn write FGotWarn;
    property GotResearch: WideString read FGotResearch write FGotResearch;
    property Sex_FORMULA : WideString read FSex_FORMULA write FSex_FORMULA;
    property Sex_SQL : WideString read GetSex_SQL;
    property Status_FORMULA: WideString read FStatus_FORMULA write FStatus_FORMULA;
    property Status_SQL : WideString read GetStatus_SQL;
  end;

implementation

{ TMembers }

function TMembers.GetSex_SQL: WideString;
begin
  Result := Format('select [NAME] from [Constants] where [Code]=''%s''', [Sex]);
end;

function TMembers.GetStatus_SQL: WideString;
begin
  Result := Format('select [NAME] from [Constants] where [Code]=''%s''', [Status]);
end;

initialization
  RegisterClass(TMembers);

end.