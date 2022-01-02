{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2022 CnPack 开发组                       }
{                   ------------------------------------                       }
{                                                                              }
{            本开发包是开源的自由软件，您可以遵照 CnPack 的发布协议来修        }
{        改和重新发布这一程序。                                                }
{                                                                              }
{            发布这一开发包的目的是希望它有用，但没有任何担保。甚至没有        }
{        适合特定目的而隐含的担保。更详细的情况请参阅 CnPack 发布协议。        }
{                                                                              }
{            您应该已经和开发包一起收到一份 CnPack 发布协议的副本。如果        }
{        还没有，可访问我们的网站：                                            }
{                                                                              }
{            网站地址：http://www.cnpack.org                                   }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}
{        该单元基于 Angus Johnson 的 TDiffUnit.pas改写，以下是原的声明：       }
(*******************************************************************************
* Component         TDiff                                                      *
* Version:          1.1                                                        *
* Date:             24 February 2002                                           *
* Compilers:        Delphi 3 - Delphi 6                                        *
* Author:           Angus Johnson - ajohnson@rpi.net.au                        *
* Copyright:        ?2001-2002 Angus Johnson                                  *
                                                                               *
* Licence to use, terms and conditions:                                        *
*                   The code in the TDiff component is released as freeware    *
*                   provided you agree to the following terms & conditions:    *
*                   1. the copyright notice, terms and conditions are          *
*                   left unchanged                                             *
*                   2. modifications to the code by other authors must be      *
*                   clearly documented and accompanied by the modifier's name. *
*                   3. the TDiff component may be freely compiled into binary  *
*                   format and no acknowledgement is required. However, a      *
*                   discrete acknowledgement would be appreciated (eg. in a    *
*                   program's 'About Box').                                    *
*                                                                              *
* Description:      Component to list differences between two integer arrays   *
*                   using a "longest common sequence" algorithm.               *
*                   Typically, this component is used to diff 2 text files     *
*                   once their individuals lines have been hashed.             *
*                   By uncommenting {$DEFINE DIFF_BYTES} this component        *
*                   can also diff char arrays (eg to create file patches)      *
*                                                                              *
* Acknowledgements: The key algorithm in this component is based on:           *
*                   "An O(ND) Difference Algorithm and its Variations"         *
*                   By E Myers - Algorithmica Vol. 1 No. 2, 1986, pp. 251-266  *
*                   http://www.cs.arizona.edu/people/gene/                     *
*                   http://www.cs.arizona.edu/people/gene/PAPERS/diff.ps       *
*                                                                              *
*******************************************************************************)

(*******************************************************************************
* History:                                                                     *
* 13 December 2001 - Original Release                                          *
* 24 February 2002 - OnProgress event added, improvements to code comments     *
*******************************************************************************)

unit CnStrDiff;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：字符串详细比较单元
* 单元作者：周劲羽 (zjy@cnpack.org)
* 备    注：该单元基于 Angus Johnson 的 TDiffUnit.pas改写
* 开发平台：PWinXP + Delphi 5.01
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2012.09.20 V1.1
*               修正Unicode版下PChar不兼容的问题
*           2004.11.15 V1.0
*               移植单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, SysUtils, Classes;

const
  //Maximum allowed deviation from centre diagonal vector ...
  MAX_DIAGONAL = $FFFFF;

type
  PDiagVectorArray = ^TDiagVectorArray;
  TDiagVectorArray = array[-MAX_DIAGONAL.. + MAX_DIAGONAL] of Integer;
  TScriptKind = (skAddRange, skDelRange, skDelDiagDel,
    skAddDiagAdd, skAddDel, skAddDiagDel, skDelDiagAdd);

  TChangeKind = (ckAdd, ckDelete, ckModify);

  PChangeRec = ^TChangeRec;
  TChangeRec = record
    Kind: TChangeKind; //(ckAdd, ckDelete, ckModify)
    x: Integer; //Array1 offset (where to add, delete, modify)
    y: Integer; //Array2 offset (what to add, modify)
    Range: Integer; //range :-)
  end;

  TProgressEvent = procedure(Sender: TObject; ProgressPercent: Integer) of object;

  TCnStrDiff = class
  private
    FMaxD: Integer;
    FChangeList: TList;
    FLastAdd, FLastDel, FLastMod: PChangeRec;
    FDiagVecB: PDiagVectorArray;
    FDiagVecF: PDiagVectorArray; //forward and backward arrays
    FArray1: PAnsiChar;
    FArray2: PAnsiChar;
    FCancelled: Boolean;

    function RecursiveDiff(x1, y1, x2, y2: Integer): Boolean;
    procedure AddToScript(x1, y1, x2, y2: Integer; ScriptKind: TScriptKind);
    procedure ClearChanges;
    function GetChangeCount: Integer;
    function GetChanges(Index: Integer): TChangeRec;
    procedure PushAdd;
    procedure PushDel;
    procedure PushMod;
  public
    constructor Create;
    destructor Destroy; override;

    function Execute(const S1, S2: PAnsiChar; Size1, Size2: Integer): Boolean;
    property ChangeCount: Integer read GetChangeCount;
    property Changes[Index: Integer]: TChangeRec read GetChanges; default;
  end;

// 计算两个字符串的相似程度，返回 0..1 之间的小数
function SimilarText(S1, S2: string; CaseSensitive: Boolean = False): Double;

implementation

// Miscellaneous Functions ...

function Min(a, b: Integer): Integer;
begin
  if a < b then Result := a else Result := b;
end;

function Max(a, b: Integer): Integer;
begin
  if a > b then Result := a else Result := b;
end;

function SimilarText(S1, S2: string; CaseSensitive: Boolean): Double;
var
  Diff: TCnStrDiff;
  i: Integer;
  Count, Len: Integer;
begin
  if (S1 = '') or (S2 = '') then
  begin
    if S1 = S2 then
      Result := 1
    else
      Result := 0;
    Exit;
  end;

  if not CaseSensitive then
  begin
    S1 := UpperCase(S1);
    S2 := UpperCase(S2);
  end;

  Diff := TCnStrDiff.Create;
  try
    if Diff.Execute(PAnsiChar(AnsiString(S1)), PAnsiChar(AnsiString(S2)), Length(S1), Length(S2)) then
    begin
      Count := 0;
      for i := 0 to Diff.ChangeCount - 1 do
        Inc(Count, Diff.Changes[i].Range);
      Len := Max(Length(S1), Length(S2));
      if Len > 1 then
        Dec(Len);
      Result := 1 - Count / Len;
      if Result < 0 then
        Result := 0;
    end
    else
      Result := 0;
  finally;
    Diff.Free;
  end;
end;

// TCnStrDiff Class ...

constructor TCnStrDiff.Create;
begin
  inherited;
  FChangeList := TList.Create;
end;

destructor TCnStrDiff.Destroy;
begin
  ClearChanges;
  FChangeList.Free;
  inherited;
end;

function TCnStrDiff.Execute(const S1, S2: PAnsiChar; Size1, Size2: Integer): Boolean;
var
  IntArr_f, IntArr_b: PAnsiChar;
begin
  Result := False;
  ClearChanges;

  if not Assigned(S1) or not Assigned(S2) then Exit;
  FArray1 := S1;
  FArray2 := S2;

  //MaxD == Maximum possible deviation from centre diagonal vector
  //which can't be more than the largest intArray (with upperlimit = MAX_DIAGONAL) ...
  FMaxD := Min(Max(Size1, Size2), MAX_DIAGONAL);

  //estimate the no. Changes == 1/8 total size rounded to a 32bit boundary
  FChangeList.Capacity := (Max(FMaxD, 1024) div 32) * 4;

  IntArr_f := nil;
  IntArr_b := nil;
  try
    //allocate the vector memory ...
    GetMem(IntArr_f, SizeOf(Integer) * (FMaxD * 2 + 1));
    GetMem(IntArr_b, SizeOf(Integer) * (FMaxD * 2 + 1));
    //Align the forward and backward diagonal vector arrays
    //with the memory which has just been allocated ...
    PAnsiChar(FDiagVecF) := PAnsiChar(IntArr_f) - SizeOf(Integer) * (MAX_DIAGONAL - FMaxD);
    PAnsiChar(FDiagVecB) := PAnsiChar(IntArr_b) - SizeOf(Integer) * (MAX_DIAGONAL - FMaxD);

    FCancelled := False;
    //NOW DO IT HERE...
    Result := RecursiveDiff(0, 0, Size1, Size2);
    //add remaining range buffers onto ChangeList...
    PushAdd;
    PushDel;

    if not Result then ClearChanges;
  finally
    FreeMem(IntArr_f);
    FreeMem(IntArr_b);
  end;
end;

function TCnStrDiff.RecursiveDiff(x1, y1, x2, y2: Integer): Boolean;
var
  //normally, parameters and local vars should be stored on the heap for
  //recursive functions. However, as the maximum number of possible recursions
  //here is relatively small (<25) the risk of stack overflow is negligible.
  x, y, Delta, D, k: Integer;
begin
  Result := True;

  //skip over initial and trailing matches...
  D := Min(x2 - x1, y2 - y1);
  k := 0;
  while (k < D) and (FArray1[x1 + k + 1] = FArray2[y1 + k + 1]) do Inc(k);
  Inc(x1, k);
  Inc(y1, k);
  Dec(D, k);
  k := 0;
  while (k < D) and (FArray1[x2 - k] = FArray2[y2 - k]) do Inc(k);
  Dec(x2, k);
  Dec(y2, k);

  //check if just all additions or all deletions...
  if (x2 = x1) then
  begin
    AddToScript(x1, y1, x2, y2, skAddRange);
    Exit;
  end else if (y2 = y1) then
  begin
    AddToScript(x1, y1, x2, y2, skDelRange);
    Exit;
  end;

  //divide and conquer ...
  //(recursively) find midpoints of the edit path...
  Delta := (x2 - x1) - (y2 - y1);
  //initialize forward and backward diagonal vectors...
  FDiagVecF[0] := x1;
  FDiagVecB[Delta] := x2;
  //OUTER LOOP ...
  //MAKE INCREASING OSCILLATIONS ABOUT CENTRE DIAGONAL UNTIL A FORWARD
  //DIAGONAL VECTOR IS GREATER THAN OR EQUAL TO A BACKWARD DIAGONAL.
  //nb: 'D' doesn't needs to start at 0 as there's never an initial match
  for D := 1 to FMaxD do
  begin
    //forward loop...............................................
    //nb: k == index of current diagonal vector and
    //    will oscillate (in increasing swings) between -FMaxD and FMaxD
    k := -D;
    while k <= D do
    begin
      //derive x from the larger of the adjacent vectors...
      if (k = -D) or ((k < D) and (FDiagVecF[k - 1] < FDiagVecF[k + 1])) then
        x := FDiagVecF[k + 1] else
        x := FDiagVecF[k - 1] + 1;
      y := x - x1 + y1 - k;
      //while (x+1,y+1) match - increment them...
      while (x < x2) and (y < y2) and (FArray1[x + 1] = FArray2[y + 1]) do
      begin
        Inc(x);
        Inc(y);
      end;
      //update current vector ...
      FDiagVecF[k] := x;

      //check if midpoint reached (ie: when diagVecF[k] & diagVecB[k] vectors overlap)...
      //nb: if midpoint found in forward loop then there must be common sub-sequences ...
      if odd(Delta) and (k > -D + Delta) and (k < D + Delta) and (FDiagVecF[k] >=
        FDiagVecB[k]) then
      begin
        //To avoid declaring 2 extra variables in this recursive function ..
        //Delta & k are simply reused to store the x & y values ...
        Delta := x;
        k := y;
        //slide up to top (left) of diagonal...
        while (x > x1) and (y > y1) and (FArray1[x] = FArray2[y]) do
        begin
          Dec(x);
          Dec(y);
        end;
        //do recursion with the first half...
        Result := RecursiveDiff(x1, y1, x, y);
        if not Result then Exit;
        //and again with the second half (nb: Delta & k are stored x & y)...
        Result := RecursiveDiff(Delta, k, x2, y2);
        Exit; //All done!!!
      end;
      Inc(k, 2);
    end;

    //backward loop..............................................
    //nb: k will oscillate (in increasing swings) between -FMaxD and FMaxD
    k := -D + Delta;

    while k <= D + Delta do
    begin
      //make sure we remain within the diagVecB[] and diagVecF[] array bounds...
      if (k < -FMaxD) then
      begin
        Inc(k, 2);
        Continue;
      end
      else if (k > FMaxD) then Break;

      //derive x from the adjacent vectors...
      if (k = D + Delta) or ((k > -D + Delta) and (FDiagVecB[k + 1] > FDiagVecB[k - 1]))
        then
        x := FDiagVecB[k - 1] else
        x := FDiagVecB[k + 1] - 1;
      y := x - x1 + y1 - k;
      //while (x,y) match - decrement them...
      while (x > x1) and (y > y1) and (FArray1[x] = FArray2[y]) do
      begin
        Dec(x);
        Dec(y);
      end;
      //update current vector ...
      FDiagVecB[k] := x;

      //check if midpoint reached...
      if not odd(Delta) and (k >= -D) and (k <= D) and (FDiagVecF[k] >= FDiagVecB[k])
        then
      begin
        //if D == 1 then the smallest common subsequence must have been found ...
        if D = 1 then //nb: if D == 1 then Delta must be in [-2,0,+2]
        begin
          if Delta = 2 then
            AddToScript(x1, y1, x2, y2, skDelDiagDel)
          else if Delta = -2 then
            AddToScript(x1, y1, x2, y2, skAddDiagAdd)
          else if (x1 + 1 = x2) then
            AddToScript(x1, y1, x2, y2, skAddDel)
          else if (FArray1[x1 + 2] = FArray2[y1 + 1]) then
            AddToScript(x1, y1, x2, y2, skDelDiagAdd)
          else
            AddToScript(x1, y1, x2, y2, skAddDiagDel);
        end else
        begin // D > 1 then find common sub-sequences...
          //process the first half...
          Result := RecursiveDiff(x1, y1, x, y);
          if not Result then Exit;
          //now slide down to bottom (right) of diagonal...
          while (x < x2) and (y < y2) and (FArray1[x + 1] = FArray2[y + 1]) do
          begin
            Inc(x);
            Inc(y);
          end;
          //and process the second half...
          Result := RecursiveDiff(x, y, x2, y2);
        end;
        Exit; //All done!!!
      end;
      Inc(k, 2);
    end;

  end;
  Result := False;
end;

(*.................................
                                  .
  skAddRange:      |              .
  (x1 == x2)       |              .
                   |              .
                                  .
  skDelRange:     ----            .
  (y1 == y2)                      .
                                  .
When the midpoint is reached in   .
the smallest possible editgrid,   .
D = 1 & Delta must be even and    .
the snake must appears as one of: .
                                  .
  skAddDiagAdd:     |             .
  (Delta == -2)      \            .
                      |           .
                                  .
  skDelDiagDel:     _             .
  (Delta == +2)      \            .
                      -           .
                                  .
  skAddDel:         |_            .
  (Delta == 0                     .
  & Rec size == 1x1)              .
  nb: skAddDel == skDelAdd        .
                                  .
  skAddDiagDel      |             .
  (Delta == 0)       \            .
                      -           .
                                  .
  skDelDiagAdd      _             .
  (Delta == 0)       \            .
                      |           .
                                  .
.................................*)

procedure TCnStrDiff.PushAdd;
begin
  PushMod;
  if Assigned(FLastAdd) then FChangeList.Add(FLastAdd);
  FLastAdd := nil;
end;

procedure TCnStrDiff.PushDel;
begin
  PushMod;
  if Assigned(FLastDel) then FChangeList.Add(FLastDel);
  FLastDel := nil;
end;

procedure TCnStrDiff.PushMod;
begin
  if Assigned(FLastMod) then FChangeList.Add(FLastMod);
  FLastMod := nil;
end;

//This is a bit UGLY but simply reduces many adds & deletes to many fewer
//add, delete & modify ranges which are then stored in ChangeList...
procedure TCnStrDiff.AddToScript(x1, y1, x2, y2: Integer; ScriptKind: TScriptKind);
var
  i: Integer;

  procedure TrashAdd;
  begin
    Dispose(FLastAdd);
    FLastAdd := nil;
  end;

  procedure TrashDel;
  begin
    Dispose(FLastDel);
    FLastDel := nil;
  end;

  procedure NewAdd(x1, y1: Integer);
  begin
    New(FLastAdd);
    FLastAdd.Kind := ckAdd;
    FLastAdd.x := x1;
    FLastAdd.y := y1;
    FLastAdd.Range := 1;
  end;

  procedure NewMod(x1, y1: Integer);
  begin
    New(FLastMod);
    FLastMod.Kind := ckModify;
    FLastMod.x := x1;
    FLastMod.y := y1;
    FLastMod.Range := 1;
  end;

  procedure NewDel(x1: Integer);
  begin
    New(FLastDel);
    FLastDel.Kind := ckDelete;
    FLastDel.x := x1;
    FLastDel.y := 0;
    FLastDel.Range := 1;
  end;

  // 1. there can NEVER be concurrent fLastAdd and fLastDel record ranges.
  // 2. fLastMod is always pushed onto ChangeList before fLastAdd & fLastDel.

  procedure Add(x1, y1: Integer);
  begin
    if Assigned(FLastAdd) then //OTHER ADDS PENDING
    begin
      if (FLastAdd.x = x1) and
        (FLastAdd.y + FLastAdd.Range = y1) then
        Inc(FLastAdd.Range) //add in series
      else
      begin
        PushAdd;
        NewAdd(x1, y1);
      end; //add NOT in series
    end
    else if Assigned(FLastDel) then //NO ADDS BUT DELETES PENDING
    begin
      if x1 = FLastDel.x then //add matches pending del so modify ...
      begin
        if Assigned(FLastMod) and (FLastMod.x + FLastMod.Range - 1 = x1) and
          (FLastMod.y + FLastMod.Range - 1 = y1) then
          Inc(FLastMod.Range) //modify in series
        else
        begin
          PushMod;
          NewMod(x1, y1);
        end; //start NEW modify

        if FLastDel.Range = 1 then TrashDel //decrement or remove existing del
        else
        begin
          Dec(FLastDel.Range);
          Inc(FLastDel.x);
        end;
      end
      else
      begin
        PushDel;
        NewAdd(x1, y1);
      end; //add does NOT match pending del's
    end
    else
      NewAdd(x1, y1); //NO ADDS OR DELETES PENDING
  end;

  procedure Delete(x1: Integer);
  begin
    if Assigned(FLastDel) then //OTHER DELS PENDING
    begin
      if (FLastDel.x + FLastDel.Range = x1) then
        Inc(FLastDel.Range) //del in series
      else
      begin
        PushDel;
        NewDel(x1);
      end; //del NOT in series
    end
    else if Assigned(FLastAdd) then //NO DELS BUT ADDS PENDING
    begin
      if x1 = FLastAdd.x then //del matches pending add so modify ...
      begin
        if Assigned(FLastMod) and (FLastMod.x + FLastMod.Range = x1) then
          Inc(FLastMod.Range) //mod in series
        else
        begin
          PushMod;
          NewMod(x1, FLastAdd.y);
        end; //start NEW modify ...
        if FLastAdd.Range = 1 then TrashAdd //decrement or remove existing add
        else
        begin
          Dec(FLastAdd.Range);
          Inc(FLastAdd.x);
          Inc(FLastAdd.y);
        end;
      end
      else
      begin
        PushAdd;
        NewDel(x1);
      end; //del does NOT match pending add's
    end
    else
      NewDel(x1); //NO ADDS OR DELETES PENDING
  end;

begin
  case ScriptKind of
    skAddRange: for i := y1 to y2 - 1 do Add(x1, i);
    skDelRange: for i := x1 to x2 - 1 do Delete(i);
    skDelDiagDel:
      begin
        Delete(x1);
        Delete(x2 - 1);
      end;
    skAddDiagAdd:
      begin
        Add(x1, y1);
        Add(x2, y2 - 1);
      end;
    skAddDel:
      begin
        Add(x1, y1);
        Delete(x2 - 1);
      end;
    skDelDiagAdd:
      begin
        Delete(x1);
        Add(x2, y2 - 1);
      end;
    skAddDiagDel:
      begin
        Add(x1, y1);
        Delete(x2 - 1);
      end;
  end;
end;

procedure TCnStrDiff.ClearChanges;
var
  i: Integer;
begin
  for i := 0 to FChangeList.Count - 1 do
    dispose(PChangeRec(FChangeList[i]));
  FChangeList.Clear;
end;

function TCnStrDiff.GetChangeCount: Integer;
begin
  Result := FChangeList.Count;
end;

function TCnStrDiff.GetChanges(Index: Integer): TChangeRec;
begin
  Result := PChangeRec(FChangeList[Index])^;
end;

end.
