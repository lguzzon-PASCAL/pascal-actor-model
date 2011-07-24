{
  This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
  the Free Software Foundation; version 2 of the License.
   
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
}

// Copyright (c) 2010 2011 - J. Aldo G. de Freitas Junior

{$MODE DELPHI}{$M+}{$H+}

Unit
	CustomActors;

Interface

Uses
	Classes,
	SysUtils,
	ActorMessages,
	Actors;

Type
	{ Actor with target }

	TWithTargetActor = Class(TActorThread)
	Private
		fTarget : String;
	Public
		Property Target : String Read fTarget;
		Procedure SetTarget(Var aMessage); Message 'tsettargetactormessage';
	End;

	{ Actor with target list }

	TWithTargetListActor = Class(TActorThread)
	Private
		fTargets : TStringList;
	Public
		Property Targets : TStringList Read fTargets;
		Constructor Create(Const aName : String = ''; CreateSuspended : Boolean = True; Const StackSize : SizeUInt = DefaultStackSize; Const aTimeout : Integer = ccDefaultTimeout); Override;
		Destructor Destroy; Override;
		Procedure AddTarget(Var aMessage); Message 'taddtargetactormessage';
		Procedure DelTarget(var aMessage); Message 'tdeletetargetactormessage';
	End;

	{ Forwarder/Cloner Actor }

	TForwardCloneActor = Class(TWithTargetListActor)
	Public
		Procedure DefaultHandlerStr(Var aMessage); Override;
	End;

	{ Round Robin Load balancer actor }

	TLoadBalancerActor = Class(TWithTargetListActor)
	Private
		fCurrent : Integer;
	Public
		Procedure DefaultHandlerStr(Var aMessage); Override;
	End;

Procedure Init;
Procedure Fini;

Procedure SetTargetOfActor(Const aName, aTarget : String);
Procedure AddTargetToActor(Const aName, aTarget : String);
Procedure DelTargetOfActor(Const aName, aTarget : String);

Implementation

// TWithTargetActor

Procedure TWithTargetActor.SetTarget(Var aMessage);
Var
	lMessage : TSetTargetActorMessage;
Begin
	lMessage := UnbundleMessage(aMessage) As TSetTargetActorMessage;
	fTarget := lMessage.Data;
End;

// TWithTargetListActor

Constructor TWithTargetListActor.Create(Const aName : String = ''; CreateSuspended : Boolean = True; Const StackSize : SizeUInt = DefaultStackSize; Const aTimeout : Integer = ccDefaultTimeout);
Begin
	Inherited Create(aName, CreateSuspended, StackSize, aTimeout);
	fTargets := TStringList.Create;
End;

Destructor TWithTargetListActor.Destroy;
Begin
	fTargets.Free;
	Inherited Destroy;
End;

Procedure TWithTargetListActor.AddTarget(Var aMessage);
Var
	lMessage : TAddTargetActorMessage;
Begin
	lMessage := UnbundleMessage(aMessage) As TAddTargetActorMessage;
	fTargets.Add(lMessage.Data);
End;

Procedure TWithTargetListActor.DelTarget(var aMessage);
Var
	lMessage : TDeleteTargetActorMessage;
Begin
	Try
		lMessage := (UnbundleMessage(aMessage) As TDeleteTargetActorMessage);
		fTargets.Delete(fTargets.IndexOf(lMessage.Data));
	Except
		On E: Exception Do ;
	End;
End;

// TForwardDuplicateActor

Procedure TForwardCloneActor.DefaultHandlerStr(Var aMessage);
Var
	lCtrl : Integer;
	lMessage : TCustomActorMessage;
Begin
	lMessage := UnbundleMessage(aMessage);
	If fTargets.Count > 0 Then
		For lCtrl := 0 To fTargets.Count - 1 Do
			ForwardTo(fTargets[lCtrl], lMessage.Clone);
End;

// TLoadBalancerActor 

Procedure TLoadBalancerActor.DefaultHandlerStr(Var aMessage);
Begin
	If fTargets.Count > 0 Then
	Begin
		If fCurrent > (fTargets.Count - 1) Then
			fCurrent := 0;
		If fCurrent < 0 Then
			fCurrent := 0;
		ForwardTo(fTargets[fCurrent]);
		Inc(fCurrent);
	End;
End;

Procedure Init;
Begin
	RegisterActorClass(TForwardCloneActor);
	RegisterActorClass(TLoadBalancerActor);
End;

Procedure Fini;
Begin
End;

Procedure SetTargetOfActor(Const aName, aTarget : String);
Var
	lTarget : TSetTargetActorMessage;
Begin
	lTarget := TSetTargetActorMessage.Create(MainThreadName, aName);
	lTarget.Data := aTarget;
	Switchboard.Mailbox.Push(lTarget);
End;

Procedure AddTargetToActor(Const aName, aTarget : String);
Var
	lTarget : TAddTargetActorMessage;
Begin
	lTarget := TAddTargetActorMessage.Create(MainThreadName, aName);
	lTarget.Data := aTarget;
	Switchboard.Mailbox.Push(lTarget);
End;

Procedure DelTargetOfActor(Const aName, aTarget : String);
Var
	lTarget : TDeleteTargetActorMessage;
Begin
	lTarget := TDeleteTargetActorMessage.Create(MainThreadName, aName);
	lTarget.Data := aTarget;
	Switchboard.Mailbox.Push(lTarget);
End;

End.
