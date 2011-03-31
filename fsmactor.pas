{
	This file is part of Pascal-Actor-Model.

	Pascal-Actor-Model is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 2 of the License, or
	(at your option) any later version.

	Pascal-Actor-Model is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with Pascal-Actor-Model.  If not, see <http://www.gnu.org/licenses/>.
}

Unit
	FSMActor;

Interface

{$MODE DELPHI}{$M+}{$H+}

Uses
	{$IFDEF UNIX}
	CThreads,
	{$ENDIF}
	Classes,
	SysUtils,
	SyncObjs,
	ContNrs,
	Actors;

Type
	TCustomState = Class;
	TCustomTransition = Class;
	TCustomStateMachine = Class;
	TCustomStateClass = Class Of TCustomState;
	TCustomTransitionClass = Class Of TCustomTransition;
	
	TCustomState = Class
	Public
		Class Procedure OnEnter(Const aMachine : TCustomStateMachine; Const aMessage : TCustomMessage); Virtual; Abstract;
		Class Procedure OnLeave(Const aMachine : TCustomStateMachine; Const aMessage : TCustomMessage); Virtual; Abstract;
	End;
	
	TInitialState = Class(TCustomState);
	TFinalState = Class(TCustomState);
	
	TCustomTransition = Class
	Public		
		Class Function IsTransitOf(Const aOlderState : TCustomStateClass; Const aMessage : TCustomMessage): Boolean; Virtual; Abstract; Overload;
		Class Function IsTransitOf(Const aOlderState, aNewerState : TCustomStateClass): Boolean; Virtual; Abstract; Overload;
		Class Function Transit(Const aMachine : TCustomStateMachine; Const aMessage : TCustomMessage): TCustomStateClass; Virtual; Abstract;
	End;
	
	TCustomStateMachine = Class(TActorThread)
	Private
		fInitialState,
		fCurrentState        : TCustomStateClass;
		fPossibleStates      : Array Of TCustomStateClass;
		fPossibleTransitions : Array Of TCustomTransitionClass;
	Public
		Constructor Create(Const aName : String = ''; CreateSuspended : Boolean = True; Const StackSize : SizeUInt = DefaultStackSize; Const aTimeout : Integer = ccDefaultTimeout); Override;
		Procedure AddState(Const aStateClass : TCustomStateClass);
		Procedure AddTransition(Const aTransitionClass : TCustomTransitionClass);
		Procedure Transit(Const aMessage : TCustomMessage);
		Procedure DefaultHandlerStr(Var aMessage); Override;
		Property Initial : TCustomStateClass Read fInitialState Write fInitialState;
		Property State : TCustomStateClass Read fCurrentState Write fCurrentState;
	End;

Implementation

Constructor TCustomStateMachine.Create(Const aName : String = ''; CreateSuspended : Boolean = True; Const StackSize : SizeUInt = DefaultStackSize; Const aTimeout : Integer = ccDefaultTimeout);
Begin
	Inherited Create(aName, CreateSuspended, StackSize, aTimeout);
	fCurrentState := Nil;
	fInitialState := Nil;
End;

Procedure TCustomStateMachine.AddState(Const aStateClass : TCustomStateClass);
Begin
	SetLength(fPossibleStates, Length(fPossibleStates) + 1);
	fPossibleStates[High(fPossibleStates)] := aStateClass;
	If aStateClass.InheritsFrom(TInitialState) Then
	Begin
		fInitialState := aStateClass;
		fCurrentState := fInitialState;
		fCurrentState.OnEnter(Self, Nil);
	End;
End;

Procedure TCustomStateMachine.AddTransition(Const aTransitionClass : TCustomTransitionClass);
Begin
	SetLength(fPossibleTransitions, Length(fPossibleTransitions) + 1);
	fPossibleTransitions[High(fPossibleTransitions)] := aTransitionClass;
End;

Procedure TCustomStateMachine.Transit(Const aMessage : TCustomMessage);
Var
	lCtrl : Integer;
Begin
	For lCtrl := Low(fPossibleTransitions) To High(fPossibleTransitions) Do
		If fPossibleTransitions[lCtrl].IsTransitOf(fCurrentState, aMessage) Then
		Begin
			fCurrentState.OnLeave(Self, aMessage);
			fCurrentState := fPossibleTransitions[lCtrl].Transit(Self, aMessage);
			fCurrentState.OnEnter(Self, aMessage);
			Break;
		End;
	If fCurrentState.InheritsFrom(TFinalState) Then
		For lCtrl := Low(fPossibleTransitions) To High(fPossibleTransitions) Do
			If fPossibleTransitions[lCtrl].IsTransitOf(fCurrentState, fInitialState) Then
			Begin
				fCurrentState.OnLeave(Self, Nil);
				fCurrentState := fPossibleTransitions[lCtrl].Transit(Self, Nil);
				fCurrentState.OnEnter(Self, Nil);
				Break;
			End;
End;

Procedure TCustomStateMachine.DefaultHandlerStr(Var aMessage);
Begin
	Transit(UnbundleMessage(aMessage));
End;

End.