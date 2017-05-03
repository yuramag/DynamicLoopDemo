program DynamicLoopDemo;

{$APPTYPE CONSOLE}

uses
  SysUtils, Classes, Variants;

type
  TDynStringArray = array of string;

function IncrementLoopState(var ACounters, ABounds: array of Integer; var ALoopIndex: Integer): Boolean;
begin
  Result := False;
  if ALoopIndex < 0 then
    Exit;

  ACounters[ALoopIndex] := ACounters[ALoopIndex] + 1;

  if ACounters[ALoopIndex] >= ABounds[ALoopIndex] then
  begin
    ACounters[ALoopIndex] := 0;
    Dec(ALoopIndex);
    Result := IncrementLoopState(ACounters, ABounds, ALoopIndex);
    Inc(ALoopIndex);
  end
  else
    Result := True;
end;

procedure IterateDynamicLoop(AList: TStrings; AData: Variant);
var
  I, lMaxCount, lLoopIndex: Integer;
  lCounters, lBounds: array of Integer;
  lItem: string;
begin
  lMaxCount := VarArrayHighBound(AData, 1) + 1;
  lLoopIndex := lMaxCount - 1;
  SetLength(lCounters, lMaxCount);
  SetLength(lBounds, lMaxCount);

  for I := 0 to lMaxCount - 1 do
  begin
    lCounters[I] := 0;
    lBounds[I] := VarArrayHighBound(AData[I], 1) + 1;
  end;

  repeat
    lItem := '';
    for I := VarArrayLowBound(AData, 1) to VarArrayHighBound(AData, 1) do
    begin
      if lItem <> '' then
        lItem := lItem + '-';
      lItem := lItem + AData[I][lCounters[I]];
    end;
    AList.Add(lItem);
  until not IncrementLoopState(lCounters, lBounds, lLoopIndex);
end;

procedure RunDemo;
var
  lData: Variant;
  lList: TStrings;
  I: Integer;
begin
  lData := VarArrayOf([
    VarArrayOf(['red', 'green', 'blue']),
    VarArrayOf(['apple', 'banana', 'peach', 'mellon']),
    VarArrayOf(['one', 'two'])]);

  lList := TStringList.Create;
  try
    IterateDynamicLoop(lList, lData);
    for I := 0 to lList.Count - 1 do
      WriteLn(lList[I]);
  finally
    lList.Free;
  end;
end;

begin
  RunDemo;
  ReadLn;
end.
