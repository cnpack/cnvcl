{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     ÖÐ¹úÈË×Ô¼ºµÄ¿ª·ÅÔ´ÂëµÚÈý·½¿ª·¢°ü                         }
{                   (C)Copyright 2001-2009 CnPack ¿ª·¢×é                       }
{                   ------------------------------------                       }
{                                                                              }
{            ±¾¿ª·¢°üÊÇ¿ªÔ´µÄ×ÔÓÉÈí¼þ£¬Äú¿ÉÒÔ×ñÕÕ CnPack µÄ·¢²¼Ð­ÒéÀ´ÐÞ        }
{        ¸ÄºÍÖØÐÂ·¢²¼ÕâÒ»³ÌÐò¡£                                                }
{                                                                              }
{            ·¢²¼ÕâÒ»¿ª·¢°üµÄÄ¿µÄÊÇÏ£ÍûËüÓÐÓÃ£¬µ«Ã»ÓÐÈÎºÎµ£±£¡£ÉõÖÁÃ»ÓÐ        }
{        ÊÊºÏÌØ¶¨Ä¿µÄ¶øÒþº¬µÄµ£±£¡£¸üÏêÏ¸µÄÇé¿öÇë²ÎÔÄ CnPack ·¢²¼Ð­Òé¡£        }
{                                                                              }
{            ÄúÓ¦¸ÃÒÑ¾­ºÍ¿ª·¢°üÒ»ÆðÊÕµ½Ò»·Ý CnPack ·¢²¼Ð­ÒéµÄ¸±±¾¡£Èç¹û        }
{        »¹Ã»ÓÐ£¬¿É·ÃÎÊÎÒÃÇµÄÍøÕ¾£º                                            }
{                                                                              }
{            ÍøÕ¾µØÖ·£ºhttp://www.cnpack.org                                   }
{            µç×ÓÓÊ¼þ£ºmaster@cnpack.org                                       }
{                                                                              }
{******************************************************************************}
{        ¸Ãµ¥Ôª»ùÓÚ Angus Johnson µÄ TDiffUnit.pas¸ÄÐ´£¬ÒÔÏÂÊÇÔ­µÄÉùÃ÷£º       }
(*******************************************************************************
* Component         TDiff                                                      *
* Version:          1.1                                                        *
* Date:             24 February 2002                                           *
* Compilers:        Delphi 3 - Delphi 6                                        *
* Author:           Angus Johnson - ajohnson@rpi.net.au                        *
* Copyright:        © 2001-2002 Angus Johnson                                  *
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
* Èí¼þÃû³Æ£º¿ª·¢°ü»ù´¡¿â
* µ¥ÔªÃû³Æ£º×Ö·û´®ÏêÏ¸±È½Ïµ¥Ôª
* µ¥Ôª×÷Õß£ºÖÜ¾¢Óð (zjy@cnpack.org)
* ±¸    ×¢£º¸Ãµ¥Ôª»ùÓÚ Angus Johnson µÄ TDiffUnit.pas¸ÄÐ´
* ¿ª·¢Æ½Ì¨£ºPWinXP + Delphi 5.01
* ¼æÈÝ²âÊÔ£ºPWin9X/2000/XP + Delphi 5/6
* ±¾ µØ »¯£º¸Ãµ¥ÔªÖÐµÄ×Ö·û´®¾ù·ûºÏ±¾µØ»¯´¦Àí·½Ê½
* µ¥Ôª±êÊ¶£º$Id$
* ÐÞ¸Ä¼ÇÂ¼£º2004.11.15 V1.0
*               ÒÆÖ²µ¥Ôª
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
    MaxD: Integer;
    fChangeList: TList;
    fLastAdd, fLastDel, fLastMod: PChangeRec;
    diagVecB: PDiagVectorArray;
    diagVecF: PDiagVectorArray; //forward and backward arrays
    Array1: PChar;
    Array2: PChar;
    fCancelled: Boolean;

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

    function Execute(const S1, S2: PChar; Size1, Size2: Integer): Boolean;
    property ChangeCount: Integer read GetChangeCount;
    property Changes[Index: Integer]: TChangeRec read GetChanges; default;
  end;

// ¼ÆËãÁ½¸ö×Ö·û´®µÄÏàËÆ³Ì¶È£¬·µ»Ø 0..1 Ö®¼äµÄÐ¡Êý
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
    if Diff.Execute(PChar(S1), PChar(S2), Length(S1), Length(S2)) then
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
  fChangeList := TList.Create;
end;

destructor TCnStrDiff.Destroy;
begin
  ClearChanges;
  fChangeList.Free;
  inherited;
end;

function TCnStrDiff.Execute(const S1, S2: PChar; Size1, Size2: Integer): Boolean;
var
  IntArr_f, IntArr_b: PChar;
begin
  Result := False;
  ClearChanges;

  if not Assigned(S1) or not Assigned(S2) then Exit;
  Array1 := S1;
  Array2 := S2;

  //MaxD == Maximum possible deviation from centre diagonal vector
  //which can't be more than the largest intArray (with upperlimit = MAX_DIAGONAL) ...
  MaxD := Min(Max(Size1, Size2), MAX_DIAGONAL);

  //estimate the no. Changes == 1/8 total size rounded to a 32bit boundary
  fChangeList.capacity := (Max(MaxD, 1024) div 32) * 4;

  IntArr_f := nil;
  IntArr_b := nil;
  try
    //allocate the vector memory ...
    GetMem(IntArr_f, SizeOf(Integer) * (MaxD * 2 + 1));
    GetMem(IntArr_b, SizeOf(Integer) * (MaxD * 2 + 1));
    //Align the forward and backward diagonal vector arrays
    //with the memory which has just been allocated ...
    PChar(diagVecF) := PChar(IntArr_f) - SizeOf(Integer) * (MAX_DIAGONAL - MaxD);
    PChar(diagVecB) := PChar(IntArr_b) - SizeOf(Integer) * (MAX_DIAGONAL - MaxD);

    fCancelled := False;
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
  while (k < D) and (Array1[x1 + k + 1] = Array2[y1 + k + 1]) do Inc(k);
  Inc(x1, k);
  Inc(y1, k);
  Dec(D, k);
  k := 0;
  while (k < D) and (Array1[x2 - k] = Array2[y2 - k]) do Inc(k);
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
  diagVecF[0] := x1;
  diagVecB[Delta] := x2;
  //OUTER LOOP ...
  //MAKE INCREASING OSCILLATIONS ABOUT CENTRE DIAGONAL UNTIL A FORWARD
  //DIAGONAL VECTOR IS GREATER THAN OR EQUAL TO A BACKWARD DIAGONAL.
  //nb: 'D' doesn't needs to start at 0 as there's never an initial match
  for D := 1 to MaxD do
  begin
    //forward loop...............................................
    //nb: k == index of current diagonal vector and
    //    will oscillate (in increasing swings) between -MaxD and MaxD
    k := -D;
    while k <= D do
    begin
      //derive x from the larger of the adjacent vectors...
      if (k = -D) or ((k < D) and (diagVecF[k - 1] < diagVecF[k + 1])) then
        x := diagVecF[k + 1] else
        x := diagVecF[k - 1] + 1;
      y := x - x1 + y1 - k;
      //while (x+1,y+1) match - increment them...
      while (x < x2) and (y < y2) and (Array1[x + 1] = Array2[y + 1]) do
      begin
        Inc(x);
        Inc(y);
      end;
      //update current vector ...
      diagVecF[k] := x;

      //check if midpoint reached (ie: when diagVecF[k] & diagVecB[k] vectors overlap)...
      //nb: if midpoint found in forward loop then there must be common sub-sequences ...
      if odd(Delta) and (k > -D + Delta) and (k < D + Delta) and (diagVecF[k] >=
        diagVecB[k]) then
      begin
        //To avoid declaring 2 extra variables in this recursive function ..
        //Delta & k are simply reused to store the x & y values ...
        Delta := x;
        k := y;
        //slide up to top (left) of diagonal...
        while (x > x1) and (y > y1) and (Array1[x] = Array2[y]) do
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
    //nb: k will oscillate (in increasing swings) between -MaxD and MaxD
    k := -D + Delta;

    while k <= D + Delta do
    begin
      //make sure we remain within the diagVecB[] and diagVecF[] array bounds...
      if (k < -MaxD) then
      begin
        Inc(k, 2);
        Continue;
      end
      else if (k > MaxD) then Break;

      //derive x from the adjacent vectors...
      if (k = D + Delta) or ((k > -D + Delta) and (diagVecB[k + 1] > diagVecB[k - 1]))
        then
        x := diagVecB[k - 1] else
        x := diagVecB[k + 1] - 1;
      y := x - x1 + y1 - k;
      //while (x,y) match - decrement them...
      while (x > x1) and (y > y1) and (Array1[x] = Array2[y]) do
      begin
        Dec(x);
        Dec(y);
      end;
      //update current vector ...
      diagVecB[k] := x;

      //check if midpoint reached...
      if not odd(Delta) and (k >= -D) and (k <= D) and (diagVecF[k] >= diagVecB[k])
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
          else if (Array1[x1 + 2] = Array2[y1 + 1]) then
            AddToScript(x1, y1, x2, y2, skDelDiagAdd)
          else
            AddToScript(x1, y1, x2, y2, skAddDiagDel);
        end else
        begin // D > 1 then find common sub-sequences...
          //process the first half...
          Result := RecursiveDiff(x1, y1, x, y);
          if not Result then Exit;
          //now slide down to bottom (right) of diagonal...
          while (x < x2) and (y < y2) and (Array1[x + 1] = Array2[y + 1]) do
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
  if Assigned(fLastAdd) then fChangeList.Add(fLastAdd);
  fLastAdd := nil;
end;

procedure TCnStrDiff.PushDel;
begin
  PushMod;
  if Assigned(fLastDel) then fChangeList.Add(fLastDel);
  fLastDel := nil;
end;

procedure TCnStrDiff.PushMod;
begin
  if Assigned(fLastMod) then fChangeList.Add(fLastMod);
  fLastMod := nil;
end;

//This is a bit UGLY but simply reduces many adds & deletes to many fewer
//add, delete & modify ranges which are then stored in ChangeList...
procedure TCnStrDiff.AddToScript(x1, y1, x2, y2: Integer; ScriptKind: TScriptKind);
var
  i: Integer;

  procedure TrashAdd;
  begin
    Dispose(fLastAdd);
    fLastAdd := nil;
  end;

  procedure TrashDel;
  begin
    Dispose(fLastDel);
    fLastDel := nil;
  end;

  procedure NewAdd(x1, y1: Integer);
  begin
    New(fLastAdd);
    fLastAdd.Kind := ckAdd;
    fLastAdd.x := x1;
    fLastAdd.y := y1;
    fLastAdd.Range := 1;
  end;

  procedure NewMod(x1, y1: Integer);
  begin
    New(fLastMod);
    fLastMod.Kind := ckModify;
    fLastMod.x := x1;
    fLastMod.y := y1;
    fLastMod.Range := 1;
  end;

  procedure NewDel(x1: Integer);
  begin
    New(fLastDel);
    fLastDel.Kind := ckDelete;
    fLastDel.x := x1;
    fLastDel.y := 0;
    fLastDel.Range := 1;
  end;

  // 1. there can NEVER be concurrent fLastAdd and fLastDel record ranges.
  // 2. fLastMod is always pushed onto ChangeList before fLastAdd & fLastDel.

  procedure Add(x1, y1: Integer);
  begin
    if Assigned(fLastAdd) then //OTHER ADDS PENDING
    begin
      if (fLastAdd.x = x1) and
        (fLastAdd.y + fLastAdd.Range = y1) then
        Inc(fLastAdd.Range) //add in series
      else
      begin
        PushAdd;
        NewAdd(x1, y1);
      end; //add NOT in series
    end
    else if Assigned(fLastDel) then //NO ADDS BUT DELETES PENDING
    begin
      if x1 = fLastDel.x then //add matches pending del so modify ...
      begin
        if Assigned(fLastMod) and (fLastMod.x + fLastMod.Range - 1 = x1) and
          (fLastMod.y + fLastMod.Range - 1 = y1) then
          Inc(fLastMod.Range) //modify in series
        else
        begin
          PushMod;
          NewMod(x1, y1);
        end; //start NEW modify

        if fLastDel.Range = 1 then TrashDel //decrement or remove existing del
        else
        begin
          Dec(fLastDel.Range);
          Inc(fLastDel.x);
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
    if Assigned(fLastDel) then //OTHER DELS PENDING
    begin
      if (fLastDel.x + fLastDel.Range = x1) then
        Inc(fLastDel.Range) //del in series
      else
      begin
        PushDel;
        NewDel(x1);
      end; //del NOT in series
    end
    else if Assigned(fLastAdd) then //NO DELS BUT ADDS PENDING
    begin
      if x1 = fLastAdd.x then //del matches pending add so modify ...
      begin
        if Assigned(fLastMod) and (fLastMod.x + fLastMod.Range = x1) then
          Inc(fLastMod.Range) //mod in series
        else
        begin
          PushMod;
          NewMod(x1, fLastAdd.y);
        end; //start NEW modify ...
        if fLastAdd.Range = 1 then TrashAdd //decrement or remove existing add
        else
        begin
          Dec(fLastAdd.Range);
          Inc(fLastAdd.x);
          Inc(fLastAdd.y);
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
  for i := 0 to fChangeList.Count - 1 do
    dispose(PChangeRec(fChangeList[i]));
  fChangeList.Clear;
end;

function TCnStrDiff.GetChangeCount: Integer;
begin
  Result := fChangeList.Count;
end;

function TCnStrDiff.GetChanges(Index: Integer): TChangeRec;
begin
  Result := PChangeRec(fChangeList[Index])^;
end;

end.
