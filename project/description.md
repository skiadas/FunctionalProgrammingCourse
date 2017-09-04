# Project Description

-


## Overall description

We will build a text-based role-playing game (RPG). The player will be able to create characters of various classes and races, and engage in combat with AI-generated opponents.

We have top-level modules that then contain smaller submodules.

- [Unit](#unit) manages all information related to playable as well as AI-generated units.
- [Equipment](#equipment) manages information related to equipment, weaponry and armor that one may carry.
- [Combat](#combat) manages the mechanics involved with hostile encounters, such as forming parties, managing actions and their order, managing in-combat unit health and buffs.
- [AI](#ai) is responsible for the management of enemy behavior.
- [UI](#ui) is responsible for user interaction, showing progress to the user and receiving input.

### Unit Module

The Unit module manages the different characteristics of the various units. A unit


```
Unit
Unit.Class
Unit.Race
Unit.Stats
Unit.Ability
Unit.Gear
Unit.Backpack
Equipment
Combat
Combat.State
Combat.Unit
Combat.Party
Combat.Effect   (Buff/Debuff)
Combat.Turn?
Combat.Action
AI
AI.Random
AI.Unit
AI.Strategy
UI
UI.Loop
UI.Menu
UI.Combat.Unit
UI.Combat.Action
```
