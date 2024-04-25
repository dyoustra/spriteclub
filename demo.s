.segment "HEADER"
	.byte "NES"		;identification string
	.byte $1A
	.byte $02		;amount of PRG ROM in 16K units
	.byte $01		;amount of CHR ROM in 8K units
	.byte $00		;mapper and mirroing
	.byte $00, $00, $00, $00
	.byte $00, $00, $00, $00, $00
.segment "ZEROPAGE"
player_x:	.RES 1	;reserves 1 byte of memory for player's x coordinate
player_y:	.RES 1  ;same but for y
player_is_walking: .byte 0  ;not walking = 0	walking = 1
player_walk_frame_counter: .byte 8 ;stores the frame of the player walk animation
player_is_rolling: .byte 0
player_roll_frame_counter: .byte 0
player_roll_direction_x: .byte 0
player_is_moving_horizontally: .byte 0

; reserving memory for spear sprites, frame counter, and basic attacking flag
spear_tip_x: .RES 1
spear_tip_y: .RES 1
spear_base_x: .RES 1
spear_base_y: .RES 1
spear_swoosh_x: .RES 1
spear_swoosh_y: .RES 1
player_is_basic_attacking: .byte 0
basic_attack_frame_counter: .byte 0 
spear_tip_tile_index: .byte 0
spear_base_tile_index: .byte 0
spear_swoosh_tile_index: .byte 0
spear_is_active: .byte 0

; CHASE ENEMIES
enemy_1_x: .RES 1
enemy_1_y: .RES 1
enemy_1_offset_x: .byte 0
enemy_1_offset_y: .byte 0
enemy_1_initialized: .byte 0
enemy_1_active: .byte 0
enemy_1_spawn_counter: .byte 0
enemy_1_despawn_counter: .byte 0
enemy_1_respawn_timer: .byte 0

enemy_2_x: .RES 1
enemy_2_y: .RES 1
enemy_2_offset_x: .byte 0
enemy_2_offset_y: .byte 0
enemy_2_initialized: .byte 0
enemy_2_active: .byte 0
enemy_2_spawn_counter: .byte 0
enemy_2_despawn_counter: .byte 0
enemy_2_respawn_timer: .byte 0

enemy_3_x: .RES 1
enemy_3_y: .RES 1
enemy_3_offset_x: .byte 0
enemy_3_offset_y: .byte 0
enemy_3_initialized: .byte 0
enemy_3_active: .byte 0
enemy_3_spawn_counter: .byte 0
enemy_3_despawn_counter: .byte 0
enemy_3_respawn_timer: .byte 0

; PROJECTILE ENEMIES
enemy_4_x: .RES 1
enemy_4_y: .RES 1
enemy_4_target_x: .byte 0
enemy_4_target_y: .byte 0
enemy_4_initialized: .byte 0
enemy_4_active: .byte 0
enemy_4_spawn_counter: .byte 0
enemy_4_despawn_counter: .byte 0
enemy_4_respawn_timer: .byte 0
bone_1_vel_x: .RES 1
bone_1_vel_y: .RES 1

enemy_5_x: .RES 1
enemy_5_y: .RES 1
enemy_5_target_x: .byte 0
enemy_5_target_y: .byte 0
enemy_5_initialized: .byte 0
enemy_5_active: .byte 0
enemy_5_spawn_counter: .byte 0
enemy_5_despawn_counter: .byte 0
enemy_5_respawn_timer: .byte 0
bone_2_vel_x: .RES 1
bone_2_vel_y: .RES 1

enemy_6_x: .RES 1
enemy_6_y: .RES 1
enemy_6_target_x: .byte 0
enemy_6_target_y: .byte 0
enemy_6_initialized: .byte 0
enemy_6_active: .byte 0
enemy_6_spawn_counter: .byte 0
enemy_6_despawn_counter: .byte 0
enemy_6_respawn_timer: .byte 0
bone_3_vel_x: .RES 1
bone_3_vel_y: .RES 1

; enemy handler subroutine variables
_enemy_x: .RES 1 ; must update enemy_X_x at the end
_enemy_y: .RES 1 ; must update enemy_X_y at the end
_enemy_frame_counter: .byte 0 ; readonly, used to decide frame index
_enemy_offset_x: .RES 1 ; readonly
_enemy_offset_y: .RES 1 ; readonly
_enemy_target_x: .RES 1 ; used internally
_enemy_target_y: .RES 1 ; used internally
_enemy_sprite_tile_index: .RES 1 ; "return" variable, used after subroutine

_projectile_enemy_x: .RES 1
_projectile_enemy_y: .RES 1
_projectile_enemy_target_x: .RES 1
_projectile_enemy_target_y: .RES 1
_bone_vel_x: .byte 0
_bone_vel_y: .byte 0
_bone_frame_counter: .byte 0

_enemy_subroutine_counter: .byte 0

_spawn_timer_lo: .byte 0
_spawn_timer_hi: .byte 0

_global_spawn_y: .RES 1
_global_spawn_x: .RES 1

seed: .RES 2 ; used for random number generation

; collision subroutine variables
_A_topleft_x: .RES 1
_A_topleft_y: .RES 1
_A_bottomright_x: .RES 1
_A_bottomright_y: .RES 1
_B_topleft_x: .RES 1
_B_topleft_y: .RES 1
_B_bottomright_x: .RES 1
_B_bottomright_y: .RES 1

has_collided: .RES 1 ; note: unless health system is implemented, these variables can be the same 
game_over: .RES 1
.segment "STARTUP"

RESET:
	SEI 		;disables interupts
	CLD			;turn off decimal mode
	
	LDX #%1000000	;disable sound IRQ
	STX $4017
	LDX #$00
	STX $4010		;disable PCM
	
	;initialize the stack register
	LDX #$FF
	TXS 		;transfer x to the stack
	
	; Clear PPU registers
	LDX #$00
	STX $2000
	STX $2001
	
	;WAIT FOR VBLANK
:
	BIT $2002
	BPL :-
	
	;CLEARING 2K MEMORY
	TXA
CLEARMEMORY:		;$0000 - $07FF
	STA $0000, X
	STA $0100, X
	STA $0300, X
	STA $0400, X
	STA $0500, X
	STA $0600, X
	STA $0700, X
		LDA #$FF
		STA $0200, X
		LDA #$00
	INX
	CPX #$00
	BNE CLEARMEMORY

	;WAIT FOR VBLANK
:
	BIT $2002
	BPL :-
	
	;SETTING SPRITE RANGE
	LDA #$02
	STA $4014
	NOP
	
	LDA #$3F	;$3F00
	STA $2006
	LDA #$00
	STA $2006
	
	LDX #$00
LOADPALETTES:
	LDA PALETTEDATA, X
	STA $2007
	INX
	CPX #$20
	BNE LOADPALETTES

;LOADING SPRITES
	LDX #$00
LOADSPRITES:
	LDA SPRITEDATA, X
	STA $0200, X
	INX
	CPX #$60	; Allocate space for 24 sprites
	BNE LOADSPRITES	

;LOADING BACKGROUND
	
LOADBACKGROUND:
	LDA $2002		;read PPU status to reset high/low latch
	LDA #$20
	STA $2006
	LDA #$00
	STA $2006
	LDX #$00
LOADBACKGROUNDP1:
	LDA BACKGROUNDDATA, X
	STA $2007
	INX
	CPX #$00
	BNE LOADBACKGROUNDP1
LOADBACKGROUNDP2:
	LDA BACKGROUNDDATA+256, X
	STA $2007
	INX
	CPX #$00
	BNE LOADBACKGROUNDP2

;LOAD BACKGROUND PALETTEDATA
	LDA #$23	;$23C0
	STA $2006
	LDA #$C0
	STA $2006
	LDX #$00
LOADBACKGROUNDPALETTEDATA:
	LDA BACKGROUNDPALETTEDATA, X
	STA $2007
	INX
	CPX #$20
	BNE LOADBACKGROUNDPALETTEDATA

	;RESET SCROLL
	LDA #$00
	STA $2005
	STA $2005	
		
;ENABLE INTERUPTS
	CLI
	
	LDA #%10010000
	STA $2000			;WHEN VBLANK OCCURS CALL NMI
	
	LDA #%00011110		;show sprites and background
	STA $2001

	; INITIALIZE VARIABLES
	LDA #$13 ; seed to use
	STA seed

	LDA #$30
	STA _global_spawn_y
	LDA #$1A
	STA _global_spawn_x

	LDA $0203
	STA player_x
	LDA $0200
	STA player_y

	LDA $0207
	STA spear_tip_x 
	LDA $0204
	STA spear_tip_y

	LDA $020B
	STA spear_base_x 
	LDA $0208
	STA spear_base_y
	
	LDA $0220
	STA enemy_1_y
	LDA $0223
	STA enemy_1_x

	LDA $0224
	STA enemy_2_y
	LDA $0227
	STA enemy_2_x

	LDA $0228
	STA enemy_3_y
	LDA $022B
	STA enemy_3_x
	
	LDA #16
	STA enemy_1_offset_x
	STA enemy_2_offset_y
	LDA #%11110000
	STA enemy_1_offset_y
	STA enemy_2_offset_x

	LDA $022C
	STA enemy_4_y
	LDA $022F
	STA enemy_4_x

	LDA $0230
	STA enemy_5_y
	LDA $0233
	STA enemy_5_x
	
	LDA $0234
	STA enemy_6_y
	LDA $0237
	STA enemy_6_x

;SET UP MUSIC

	LOAD_MUS_DATA:
		LDX #00
	MUSDATALOOP:
		LDA wismm_music_data, X
		STA $1D00, X
		INX
		CPX #$F2
		BNE MUSDATALOOP

		LDA #$01
		;LDX #.lobyte(wismm_music_data)
		;LDY #.hibyte(wismm_music_data)
		LDX #$00
		LDY #$1D
		JSR FamiToneInit

		LDA #00

		JSR FamiToneMusicPlay

	INFLOOP:
		JMP INFLOOP

NMI: ; PPU Update Loop -- gets called every frame

	LDA #$02	;LOAD SPRITE RANGE
	STA $4014

	refresh_music:
		JSR FamiToneUpdate

	LDA game_over 

	BEQ :+ 

	RTI
	
	:

	; used for animation -- by default the player is not walking
	; if the player is walking, the flag will be set when input is read
	LDA #$00
	STA player_is_walking ; mark the player as not walking
	STA player_is_moving_horizontally

	;	----------	CONTROLLER INPUTS	-----------
	;	controller input sequence: 
	;	A, B, Select, Start, Up, Down, Left, Right
	LatchController: 
		LDA #$01
		STA $4016
		LDA #$00		; what the fuck? this is necessary btw
		STA $4016       ; tell both the controllers to latch buttons

	ReadA:
		LDA $4016
		AND #%00000001
		BNE DoA
		JMP ReadADone
	DoA:
		LDA player_is_basic_attacking
		BNE ReadADone
		LDA player_is_rolling
		BNE ReadADone
		LDA #$01
		STA player_is_basic_attacking	; basic attacking = true	
	ReadADone:
	
	ReadB:
		LDA $4016
		AND #%00000001
		BNE DoB
		JMP ReadBDone
	DoB:
		LDA player_is_rolling
		BNE ReadBDone
		LDA player_roll_direction_x
		BEQ ReadBDone
		LDA #1
		STA player_is_rolling
		LDA #0
		STA player_roll_frame_counter
		STA player_roll_direction_x
		STA player_is_basic_attacking
	ReadBDone:

	ReadSelect:
		LDA $4016
		AND #%00000001
		BNE DoSelect
		JMP ReadSelectDone
	DoSelect:
		;unimplemented -- no effect on game
	ReadSelectDone:

	ReadStart:
		LDA $4016
		AND #%00000001
		BNE DoStart
		JMP ReadStartDone
	DoStart:
		;unimplemented -- no effect on game
	ReadStartDone:

	ReadUp:
		LDA $4016
		AND #%00000001
		BNE DoUp
		JMP ReadUpDone
	DoUp:
		LDA player_is_rolling
		BNE ReadUpDone

		LDA #$01
		STA player_is_walking ; mark the player as walking

		LDA basic_attack_frame_counter
		CMP #24
		BCS end_player_attack_slowdown_up
		AND #$01
		BNE ReadUpDone
		end_player_attack_slowdown_up:

		LDA player_y ; get player_y into A
		STA $0200 ; update player_y in the sprite data
		CMP #$31 ; top of the screen
		TAX
		BCC do_up_skip_decrease_player_y ; don't move up if we're at the top of the screen
		do_up_decrease_player_y:
			DEX ; moves the player up
		do_up_skip_decrease_player_y:
			STX player_y ; update our player_y variable
	ReadUpDone:

	ReadDown:
		LDA $4016
		AND #%00000001
		BNE DoDown
		JMP ReadDownDone
	DoDown:
		LDA player_is_rolling
		BNE ReadDownDone

		LDA #$01
		STA player_is_walking ; mark the player as walking

		LDA basic_attack_frame_counter
		CMP #24
		BCS end_player_attack_slowdown_down
		AND #$01
		BNE ReadDownDone
		end_player_attack_slowdown_down:

		LDA player_y ; get player_y into A
		STA $0200 ; update player_y in the sprite data
		CMP #$DF ; bottom of the screen
		TAX
		BCS do_down_skip_increase_player_y ; don't move down if we're at the bottom of the screen
		do_down_increase_player_y:
			INX ; moves the player down
		do_down_skip_increase_player_y:
			STX player_y ; update our player_y variable
	ReadDownDone:
	
	ReadLeft:
		LDA $4016
		AND #%00000001
		BNE DoLeft
		JMP ReadLeftDone
	DoLeft:
		LDA #1
		STA player_is_moving_horizontally

		; make the player face left
		LDA $0202 ; get attributes for flipping horizontally
		ORA #%01000000
		STA $0202 ; write back after ensuring sprite flip horizontal bit is 1. other bits are preserved.

		; spear_tip face left
		LDA $0206
		ORA #%01000000
		STA $0206

		; spear_base face left
		LDA $020A
		ORA #%01000000
		STA $020A

		; spear_swoosh face left
		LDA $020E
		ORA #%01000000
		STA $020E

		LDA player_roll_frame_counter
		CMP #6
		BEQ set_roll_dir_left

		LDA player_is_rolling
		BNE ReadLeftDone

		LDA #$01
		STA player_is_walking ; mark the player as walking

		LDA basic_attack_frame_counter
		CMP #24
		BCS end_player_attack_slowdown_left
		AND #$01
		BNE set_roll_dir_left
		end_player_attack_slowdown_left:

		LDA player_x ; get player_x into A
		STA $0203 ; update player_x in the sprite data
		TAX
		DEX ; moves the player left
		STX player_x ; update our player_x variable
	
		set_roll_dir_left:
			LDA #$FE
			STA player_roll_direction_x
	ReadLeftDone:

	ReadRight:
		LDA $4016
		AND #%00000001
		BNE DoRight
		JMP ReadRightDone
	DoRight:
		LDA #1
		STA player_is_moving_horizontally

		; make the player face right
		LDA $0202 ; get attributes for flipping horizontally
		AND #%10111111
		STA $0202 ; write back after ensuring sprite flip horizontal bit is 0. other bits are preserved.

		; spear_tip face right
		LDA $0206
		AND #%10111111
		STA $0206

		; spear_base face right
		LDA $020A
		AND #%10111111
		STA $020A

		; spear_swoosh face right
		LDA $020E
		AND #%10111111
		STA $020E

		LDA player_roll_frame_counter
		CMP #6
		BEQ set_roll_dir_right

		LDA player_is_rolling
		BNE ReadRightDone

		LDA #$01
		STA player_is_walking ; mark the player as walking

		LDA basic_attack_frame_counter
		CMP #24
		BCS end_player_attack_slowdown_right
		AND #$01
		BNE set_roll_dir_right
		end_player_attack_slowdown_right:

		LDA player_x ; get player_x into A
		STA $0203 ; update player_x in the sprite data
		TAX
		INX ; moves the player right
		STX player_x ; update our player_x variable

		set_roll_dir_right:
			LDA #2
			STA player_roll_direction_x
	ReadRightDone:

	; set the player animation frames
	LDA player_is_rolling
	BNE player_rolling_animation
	LDA player_is_walking
	BEQ player_idle_animation
	player_walking_animation:
		; update the frame counter
		LDA basic_attack_frame_counter
		AND #$01
		BNE skip_player_walk_frame_counter_increase
		LDX player_walk_frame_counter
		INX
		STX player_walk_frame_counter
		skip_player_walk_frame_counter_increase:
		LDA player_walk_frame_counter

		; A >> 3
		; animation changes frame every 8 real frames
		LSR
		LSR
		LSR

		AND #%00000011
		CMP #$02
		BEQ player_walking_frame_2
		AND #%00000001
		CMP #$01
		BEQ player_walking_frame_1
		JMP player_walking_frame_0

		player_walking_frame_0:
			LDA #$00 ; pick frame 0
			STA $0201 ; update the sprite
			JMP player_animation_done

		player_walking_frame_1:
			LDA #$01 ; pick frame 1
			STA $0201 ; update the sprite
			JMP player_animation_done

		player_walking_frame_2:
			LDA #$02 ; pick frame 2
			STA $0201 ; update the sprite
			JMP player_animation_done

	player_idle_animation:
		LDA #$00 
		STA $0201 ; reset the sprite to idle position
		LDA #$08
		STA player_walk_frame_counter ; reset the walk animation
		JMP player_animation_done
	player_rolling_animation:
		LDA player_roll_frame_counter
		BNE roll_skip_horizontal_input_check
		LDX player_is_moving_horizontally
		CPX #0
		BEQ player_end_roll_animation
		roll_skip_horizontal_input_check:


		CMP #36 ; stop the roll animation if needed
		BEQ player_end_roll_animation
		CMP #28
		BNE player_roll_skip_freeze
		LDX #0
		STX player_roll_direction_x
		player_roll_skip_freeze:

		LSR
		LSR
		
		CLC
		ADC #$40

		STA $0201

		; move the player in the right direction
		LDA player_x ; get player_x into A
		STA $0203 ; update player_x in the sprite data
		CLC
		ADC player_roll_direction_x
		STA player_x ; update our player_x variable

		; increment the frame counter
		LDA player_roll_frame_counter
		TAX
		INX
		STX player_roll_frame_counter
		JMP player_animation_done
	player_end_roll_animation:
		LDA #0
		STA player_is_rolling
		STA player_roll_direction_x
	player_animation_done:


	LDA #0 
	STA spear_is_active
	LDA player_is_basic_attacking
	BEQ intermediate_branch_4 ; goes to spear_idle_animation
	spear_animation:
		; load spear coordinates to memory
		LDA spear_tip_x
		STA $0207
		LDA spear_tip_y
		STA $0204
		LDA spear_base_x
		STA $020B
		LDA spear_base_y
		STA $0208
		LDA spear_swoosh_x
		STA $020F
		LDA spear_swoosh_y
		STA $020C
		LDA spear_tip_tile_index
		STA $0205
		LDA spear_base_tile_index
		STA $0209
		LDA spear_swoosh_tile_index
		STA $020D

		; update the basic attack frame counter
		LDX basic_attack_frame_counter
		INX 
		STX basic_attack_frame_counter
		TXA
		CMP #32
		BEQ intermediate_branch_4

		; A >> 3
		; animation changes frame every 8 real frames
		; any change to this also needs to change the above CMP #24 line
		LSR
		LSR
		LSR

		AND #%00000011
		CMP #$03
		BEQ intermediate_branch_5 ; goes to spear_attack_frame_3
		AND #%00000011
		CMP #$02
		BEQ intermediate_branch_5 ; goes to spear_attack_frame_3
		AND #%0000001
		CMP #$01
		BEQ spear_attack_frame_2
		AND #%00000001
		CMP #$00
		BEQ spear_attack_frame_1
		
		JMP spear_idle_animation ; this doesn't really work... hmm... TODO delete
		
		intermediate_branch_4:
			JMP spear_idle_animation

		spear_attack_frame_1: ; horizontal spear
			LDA #$15
			STA spear_tip_tile_index ; spear tip tile
			LDA #$14
			STA spear_base_tile_index ; spear base tile
			LDA #$FF
			STA spear_swoosh_tile_index ; spear swoosh tile

			LDA $0202 ; load which way player is facing
			AND #%01000000 ; get the horizontal flip bit
			BNE frame_1_face_left ; 1 means player facing left

			frame_1_face_right:
				LDA player_x
				CLC
				ADC #7
				STA spear_tip_x

				LDA player_x
				SEC
				SBC #1
				STA spear_base_x

				LDA player_y
				STA spear_base_y
				STA spear_tip_y

				JMP after_spear_animation

			frame_1_face_left:
				LDA player_x
				SEC
				SBC #7
				STA spear_tip_x

				LDA player_x
				CLC
				ADC #1
				STA spear_base_x
				
				LDA player_y
				STA spear_base_y
				STA spear_tip_y

				JMP after_spear_animation

		intermediate_branch_5:
			JMP spear_attack_frame_3

		spear_attack_frame_2: ; angle down spear
			LDA #$05
			STA spear_tip_tile_index ; spear tip tile
			LDA #$04
			STA spear_base_tile_index ; spear base tile
			LDA #$FF
			STA spear_swoosh_tile_index ; spear swoosh tile
			
			LDA $0202 ; load which way player is facing
			AND #%01000000 ; get the horizontal flip bit
			BNE frame_2_face_left ; 1 means player facing left

			frame_2_face_right:
				LDA player_x
				CLC
				ADC #6
				STA spear_tip_x

				LDA player_y
				CLC
				ADC #4
				STA spear_tip_y

				LDA player_x
				SEC
				SBC #2
				STA spear_base_x
				
				LDA player_y
				CLC
				ADC #4
				STA spear_base_y

				JMP after_spear_animation

			frame_2_face_left:
				LDA player_x
				SEC
				SBC #6
				STA spear_tip_x

				LDA player_y
				CLC
				ADC #4
				STA spear_tip_y

				LDA player_x
				CLC
				ADC #2
				STA spear_base_x

				LDA player_y
				CLC
				ADC #4
				STA spear_base_y
				
				JMP after_spear_animation

		spear_attack_frame_3: ; swoosh spear
			LDA #1
			STA spear_is_active

			LDA #$07
			STA spear_tip_tile_index ; spear tip tile
			LDA #$16
			STA spear_base_tile_index ; spear base tile
			LDA #$17
			STA spear_swoosh_tile_index ; spear swoosh tile

			LDA $0202 ; load which way player is facing
			AND #%01000000 ; get the horizontal flip bit
			BNE frame_3_face_left ; 1 means player facing left

			frame_3_face_right:
				LDA player_x
				CLC
				ADC #6
				STA spear_tip_x

				LDA player_y
				SEC
				SBC #5
				STA spear_tip_y

				LDA player_x
				SEC
				SBC #2
				STA spear_base_x

				LDA player_y
				CLC
				ADC #3
				STA spear_base_y

				LDA player_x
				CLC
				ADC #6
				STA spear_swoosh_x

				LDA player_y
				CLC
				ADC #3
				STA spear_swoosh_y

				JMP after_spear_animation

			frame_3_face_left:
				LDA player_x
				SEC
				SBC #6
				STA spear_tip_x

				LDA player_y
				SEC
				SBC #5
				STA spear_tip_y

				LDA player_x
				CLC
				ADC #2
				STA spear_base_x

				LDA player_y
				CLC
				ADC #3
				STA spear_base_y

				LDA player_x
				SEC
				SBC #6
				STA spear_swoosh_x

				LDA player_y
				CLC
				ADC #3
				STA spear_swoosh_y

				JMP after_spear_animation

	spear_idle_animation:
		LDA #$FF
		STA spear_tip_tile_index
		STA spear_base_tile_index
		STA spear_swoosh_tile_index
		STA $0205
		STA $0209
		STA $020D
		LDA player_is_rolling
		BNE after_spear_animation ; dont show spear if we're rolling
	
		; player_is_basic_attacking
		LDA #$00
		STA basic_attack_frame_counter
		STA player_is_basic_attacking

		LDA $0203 ; player_x
		STA spear_base_x
		STA $020B ; spear_base_x
		STA spear_tip_x
		STA $0207 ; spear_tip_x

		LDA $0200 ; player_y
		STA spear_base_y
		STA $0208 ; spear_base_y
		SEC
		SBC #$08
		STA spear_tip_y
		STA $0204 ; spear_tip_y

		LDA #$03
		STA spear_tip_tile_index ; spear tip tile
		STA $0205
		LDA #$13
		STA spear_base_tile_index ; spear base tile
		STA $0209
		LDA #$FF
		STA spear_swoosh_tile_index ; spear swoosh tile
		STA $020D

	after_spear_animation:

	; ENEMY MOVEMENT
	LDA #$00
	STA _enemy_subroutine_counter

	; incrememt spawn timers
	LDA _spawn_timer_lo
	BNE skip_hi_spawn_increment
	LDY _spawn_timer_hi
	INY
	STY _spawn_timer_hi
	skip_hi_spawn_increment:
	TAX
	INX
	STX _spawn_timer_lo

	; increment frame counter
	LDA _enemy_frame_counter 
	TAX
	INX
	STX _enemy_frame_counter ; increment the frame counter every frame

	ENEMY_1:
		LDA enemy_1_initialized
		BEQ enemy_1_handle_uninitalized
		LDA enemy_1_active
		BEQ enemy_1_handle_inactive

		; update enemy 1 x and y
		LDA enemy_1_x
		STA $0223
		LDA enemy_1_y
		STA $0220

		; setup input parameters
		LDA enemy_1_x
		STA _enemy_x
		LDA enemy_1_y
		STA _enemy_y
		LDA enemy_1_offset_x
		STA _enemy_offset_x
		LDA enemy_1_offset_y
		STA _enemy_offset_y

		JSR ENEMY_MOVEMENT_HANDLER

		; handle function end
		
		; update positions
		LDA _enemy_x
		STA enemy_1_x
		LDA _enemy_y
		STA enemy_1_y

		; pick the right animation frame for the enemy
		LDA _enemy_sprite_tile_index
		STA $0221
		JMP enemy_1_handle_done
	
		enemy_1_handle_uninitalized:
			LDA _spawn_timer_hi
			CMP #2 ; spawn after 2 cycles
			BNE enemy_1_not_ready_to_initalize
			LDA #1
			STA enemy_1_initialized
			LDA #0
			STA enemy_1_respawn_timer
			LDA _global_spawn_x
			STA enemy_1_x
			STA $0223 
			LDA _global_spawn_y
			STA enemy_1_y
			STA $0220
			JSR MOVE_SPAWN_POINT
			enemy_1_not_ready_to_initalize:
			JMP enemy_1_handle_done
		enemy_1_handle_inactive:
			LDA enemy_1_respawn_timer
			BNE enemy_1_wait_for_respawn

			LDA enemy_1_spawn_counter
			CMP #32
			BNE enemy_1_spawn_animation ; need to spawn
			   
			LDA enemy_1_despawn_counter
			BNE enemy_1_despawn_animation ; need to despawn

			LDA enemy_1_respawn_timer
			BNE enemy_1_wait_for_respawn

			; preemptively set the enemy's despawn counter and respawn timer
			LDA #16
			STA enemy_1_despawn_counter

			LDA #1
			STA enemy_1_active
			JMP enemy_1_handle_done
		enemy_1_spawn_animation:
			LDA enemy_1_spawn_counter 
			TAX
			INX
			STX enemy_1_spawn_counter ; increment the spawn counter
			
			LSR 
			LSR
			LSR
			LSR

			CLC
			ADC #$50
			STA $0221
			JMP enemy_1_handle_done
		enemy_1_despawn_animation:
			LDA enemy_1_despawn_counter 
			TAX
			DEX
			CPX #0
			BNE :+
				LDY #100
				STY enemy_1_respawn_timer
				LDA _global_spawn_x
				STA enemy_1_x
				STA $0223 
				LDA _global_spawn_y
				STA enemy_1_y
				STA $0220
				JSR MOVE_SPAWN_POINT
			:
			STX enemy_1_despawn_counter ; increment the spawn counter
			
			LSR 
			LSR
			LSR

			CLC
			ADC #$50
			STA $0221
			JMP enemy_1_handle_done
		enemy_1_wait_for_respawn:
			LDA enemy_1_respawn_timer
			TAX
			INX
			STX enemy_1_respawn_timer
			LDA #0
			STA enemy_1_spawn_counter
			LDA #$FF
			STA $0221

		enemy_1_handle_done:
	
	ENEMY_2:
		LDA _enemy_subroutine_counter
		TAX
		INX
		STX _enemy_subroutine_counter

		LDA enemy_2_initialized
		BEQ enemy_2_handle_uninitalized
		LDA enemy_2_active
		BEQ enemy_2_handle_inactive

		; update enemy 1 x and y
		LDA enemy_2_x
		STA $0227
		LDA enemy_2_y
		STA $0224

		; setup input parameters
		LDA enemy_2_x
		STA _enemy_x
		LDA enemy_2_y
		STA _enemy_y
		LDA enemy_2_offset_x
		STA _enemy_offset_x
		LDA enemy_2_offset_y
		STA _enemy_offset_y

		JSR ENEMY_MOVEMENT_HANDLER

		; handle function end
		
		; update positions
		LDA _enemy_x
		STA enemy_2_x
		LDA _enemy_y
		STA enemy_2_y

		; pick the right animation frame for the enemy
		LDA _enemy_sprite_tile_index
		STA $0225
		JMP enemy_2_handle_done
	
		enemy_2_handle_uninitalized:
			LDA _spawn_timer_hi
			CMP #4 ; spawn after 4 cycles
			BNE enemy_2_not_ready_to_initalize
			LDA #1
			STA enemy_2_initialized
			LDA #0
			STA enemy_2_respawn_timer
			LDA _global_spawn_x
			STA enemy_2_x
			STA $0227 
			LDA _global_spawn_y
			STA enemy_2_y
			STA $0224
			JSR MOVE_SPAWN_POINT
			enemy_2_not_ready_to_initalize:
			JMP enemy_2_handle_done
		enemy_2_handle_inactive:
			LDA enemy_2_respawn_timer
			BNE enemy_2_wait_for_respawn

			LDA enemy_2_spawn_counter
			CMP #32
			BNE enemy_2_spawn_animation ; need to spawn
			   
			LDA enemy_2_despawn_counter
			BNE enemy_2_despawn_animation ; need to despawn

			LDA enemy_2_respawn_timer
			BNE enemy_2_wait_for_respawn

			; preemptively set the enemy's despawn counter and respawn timer
			LDA #16
			STA enemy_2_despawn_counter

			LDA #1
			STA enemy_2_active
			JMP enemy_2_handle_done
		enemy_2_spawn_animation:
			LDA enemy_2_spawn_counter 
			TAX
			INX
			STX enemy_2_spawn_counter ; increment the spawn counter
			
			LSR 
			LSR
			LSR
			LSR

			CLC
			ADC #$50
			STA $0225
			JMP enemy_2_handle_done
		enemy_2_despawn_animation:
			LDA enemy_2_despawn_counter 
			TAX
			DEX
			CPX #0
			BNE :+
				LDY #100
				STY enemy_2_respawn_timer
				LDA _global_spawn_x
				STA enemy_2_x
				STA $0227 
				LDA _global_spawn_y
				STA enemy_2_y
				STA $0224
				JSR MOVE_SPAWN_POINT
			:
			STX enemy_2_despawn_counter ; increment the spawn counter
			
			LSR 
			LSR
			LSR

			CLC
			ADC #$50
			STA $0225
			JMP enemy_2_handle_done
		enemy_2_wait_for_respawn:
			LDA enemy_2_respawn_timer
			TAX
			INX
			STX enemy_2_respawn_timer
			LDA #0
			STA enemy_2_spawn_counter
			LDA #$FF
			STA $0225

		enemy_2_handle_done:

	ENEMY_3:
		LDA _enemy_subroutine_counter
		TAX
		INX
		STX _enemy_subroutine_counter

		LDA enemy_3_initialized
		BEQ enemy_3_handle_uninitalized
		LDA enemy_3_active
		BEQ enemy_3_handle_inactive

		; update enemy 1 x and y
		LDA enemy_3_x
		STA $022B
		LDA enemy_3_y
		STA $0228

		; setup input parameters
		LDA enemy_3_x
		STA _enemy_x
		LDA enemy_3_y
		STA _enemy_y
		LDA enemy_3_offset_x
		STA _enemy_offset_x
		LDA enemy_3_offset_y
		STA _enemy_offset_y

		JSR ENEMY_MOVEMENT_HANDLER

		; handle function end
		
		; update positions
		LDA _enemy_x
		STA enemy_3_x
		LDA _enemy_y
		STA enemy_3_y

		; pick the right animation frame for the enemy
		LDA _enemy_sprite_tile_index
		STA $0229
		JMP enemy_3_handle_done
	
		enemy_3_handle_uninitalized:
			LDA _spawn_timer_hi
			CMP #6 ; spawn after 6 cycles
			BNE enemy_3_not_ready_to_initalize
			LDA #1
			STA enemy_3_initialized
			LDA #0
			STA enemy_3_respawn_timer
			LDA _global_spawn_x
			STA enemy_3_x
			STA $022B 
			LDA _global_spawn_y
			STA enemy_3_y
			STA $0228
			JSR MOVE_SPAWN_POINT
			enemy_3_not_ready_to_initalize:
			JMP enemy_3_handle_done
		enemy_3_handle_inactive:
			LDA enemy_3_respawn_timer
			BNE enemy_3_wait_for_respawn

			LDA enemy_3_spawn_counter
			CMP #32
			BNE enemy_3_spawn_animation ; need to spawn
			   
			LDA enemy_3_despawn_counter
			BNE enemy_3_despawn_animation ; need to despawn

			LDA enemy_3_respawn_timer
			BNE enemy_3_wait_for_respawn

			; preemptively set the enemy's despawn counter and respawn timer
			LDA #16
			STA enemy_3_despawn_counter

			LDA #1
			STA enemy_3_active
			JMP enemy_3_handle_done
		enemy_3_spawn_animation:
			LDA enemy_3_spawn_counter 
			TAX
			INX
			STX enemy_3_spawn_counter ; increment the spawn counter
			
			LSR 
			LSR
			LSR
			LSR

			CLC
			ADC #$50
			STA $0229
			JMP enemy_3_handle_done
		enemy_3_despawn_animation:
			LDA enemy_3_despawn_counter 
			TAX
			DEX
			CPX #0
			BNE :+
				LDY #100
				STY enemy_3_respawn_timer
				LDA _global_spawn_x
				STA enemy_3_x
				STA $022B 
				LDA _global_spawn_y
				STA enemy_3_y
				STA $0228
				JSR MOVE_SPAWN_POINT
			:
			STX enemy_3_despawn_counter ; increment the spawn counter
			
			LSR 
			LSR
			LSR

			CLC
			ADC #$50
			STA $0229
			JMP enemy_3_handle_done
		enemy_3_wait_for_respawn:
			LDA enemy_3_respawn_timer
			TAX
			INX
			STX enemy_3_respawn_timer
			LDA #0
			STA enemy_3_spawn_counter
			LDA #$FF
			STA $0229

		enemy_3_handle_done:

	ENEMY_4:
		LDA _enemy_subroutine_counter
		TAX
		INX
		STX _enemy_subroutine_counter

		LDA enemy_4_initialized
		BEQ enemy_4_handle_uninitalized
		LDA enemy_4_active
		BEQ enemy_4_handle_inactive

		; update enemy x and y
		LDA enemy_4_x
		STA $022F
		LDA enemy_4_y
		STA $022C

		; setup input parameters
		LDA enemy_4_x
		STA _projectile_enemy_x
		LDA enemy_4_y
		STA _projectile_enemy_y
		LDA enemy_4_target_x
		STA _projectile_enemy_target_x
		LDA enemy_4_target_y
		STA _projectile_enemy_target_y
	
		JSR PROJECTILE_ENEMY_HANDLER

		; handle function end

		; update positions and targets
		LDA _projectile_enemy_x
		STA enemy_4_x
		LDA _projectile_enemy_y
		STA enemy_4_y
		LDA _projectile_enemy_target_x
		STA enemy_4_target_x
		LDA _projectile_enemy_target_y
		STA enemy_4_target_y

		JMP enemy_4_handle_done
	
		enemy_4_handle_uninitalized:
			LDA _spawn_timer_hi
			CMP #3 ; spawn after 3 cycles
			BNE enemy_4_not_ready_to_initalize
			LDA #1
			STA enemy_4_initialized
			LDA #0
			STA enemy_4_respawn_timer
			LDA _global_spawn_x
			STA enemy_4_x
			STA $022F
			LDA _global_spawn_y
			STA enemy_4_y
			STA $022C
			JSR MOVE_SPAWN_POINT
			enemy_4_not_ready_to_initalize:
			JMP enemy_4_handle_done
		enemy_4_handle_inactive:
			LDA enemy_4_respawn_timer
			BNE enemy_4_wait_for_respawn

			LDA enemy_4_spawn_counter
			CMP #32
			BNE enemy_4_spawn_animation ; need to spawn
			   
			LDA enemy_4_despawn_counter
			BNE enemy_4_despawn_animation ; need to despawn

			LDA enemy_4_respawn_timer
			BNE enemy_4_wait_for_respawn

			; preemptively set the enemy's despawn counter and respawn timer
			LDA #16
			STA enemy_4_despawn_counter

			LDA #$20
			STA $022D

			LDA #1
			STA enemy_4_active
			JMP enemy_4_handle_done
		enemy_4_spawn_animation:
			LDA enemy_4_spawn_counter 
			TAX
			INX
			STX enemy_4_spawn_counter ; increment the spawn counter
			
			LSR 
			LSR
			LSR
			LSR

			CLC
			ADC #$60
			STA $022D
			JMP enemy_4_handle_done
		enemy_4_despawn_animation:
			LDA enemy_4_despawn_counter 
			TAX
			DEX
			CPX #0
			BNE :+
				LDY #100
				STY enemy_4_respawn_timer
				LDA _global_spawn_x
				STA enemy_4_x
				STA $022F 
				LDA _global_spawn_y
				STA enemy_4_y
				STA $022C
				JSR MOVE_SPAWN_POINT
			:
			STX enemy_4_despawn_counter ; increment the spawn counter
			
			LSR 
			LSR
			LSR

			CLC
			ADC #$60
			STA $022D
			JMP enemy_4_handle_done
		enemy_4_wait_for_respawn:
			LDA enemy_4_respawn_timer
			TAX
			INX
			STX enemy_4_respawn_timer
			LDA #0
			STA enemy_4_spawn_counter
			LDA #$FF
			STA $022D

		enemy_4_handle_done:

	ENEMY_5:
		LDA _enemy_subroutine_counter
		TAX
		INX
		STX _enemy_subroutine_counter

		LDA enemy_5_initialized
		BEQ enemy_5_handle_uninitalized
		LDA enemy_5_active
		BEQ enemy_5_handle_inactive

		; update enemy x and y
		LDA enemy_5_x
		STA $0233
		LDA enemy_5_y
		STA $0230

		; setup input parameters
		LDA enemy_5_x
		STA _projectile_enemy_x
		LDA enemy_5_y
		STA _projectile_enemy_y
		LDA enemy_5_target_x
		STA _projectile_enemy_target_x
		LDA enemy_5_target_y
		STA _projectile_enemy_target_y
	
		JSR PROJECTILE_ENEMY_HANDLER

		; handle function end

		; update positions and targets
		LDA _projectile_enemy_x
		STA enemy_5_x
		LDA _projectile_enemy_y
		STA enemy_5_y
		LDA _projectile_enemy_target_x
		STA enemy_5_target_x
		LDA _projectile_enemy_target_y
		STA enemy_5_target_y

		JMP enemy_5_handle_done
	
		enemy_5_handle_uninitalized:
			LDA _spawn_timer_hi
			CMP #5 ; spawn after 5 cycles
			BNE enemy_5_not_ready_to_initalize
			LDA #1
			STA enemy_5_initialized
			LDA #0
			STA enemy_5_respawn_timer
			LDA _global_spawn_x
			STA enemy_5_x
			STA $0233
			LDA _global_spawn_y
			STA enemy_5_y
			STA $0230
			JSR MOVE_SPAWN_POINT
			enemy_5_not_ready_to_initalize:
			JMP enemy_5_handle_done
		enemy_5_handle_inactive:
			LDA enemy_5_respawn_timer
			BNE enemy_5_wait_for_respawn

			LDA enemy_5_spawn_counter
			CMP #32
			BNE enemy_5_spawn_animation ; need to spawn
			   
			LDA enemy_5_despawn_counter
			BNE enemy_5_despawn_animation ; need to despawn

			LDA enemy_5_respawn_timer
			BNE enemy_5_wait_for_respawn

			; preemptively set the enemy's despawn counter and respawn timer
			LDA #16
			STA enemy_5_despawn_counter

			LDA #$20
			STA $0231

			LDA #1
			STA enemy_5_active
			JMP enemy_5_handle_done
		enemy_5_spawn_animation:
			LDA enemy_5_spawn_counter 
			TAX
			INX
			STX enemy_5_spawn_counter ; increment the spawn counter
			
			LSR 
			LSR
			LSR
			LSR

			CLC
			ADC #$60
			STA $0231
			JMP enemy_5_handle_done
		enemy_5_despawn_animation:
			LDA enemy_5_despawn_counter 
			TAX
			DEX
			CPX #0
			BNE :+
				LDY #100
				STY enemy_5_respawn_timer
				LDA _global_spawn_x
				STA enemy_5_x
				STA $0233 
				LDA _global_spawn_y
				STA enemy_5_y
				STA $0230
				JSR MOVE_SPAWN_POINT
			:
			STX enemy_5_despawn_counter ; increment the spawn counter
			
			LSR 
			LSR
			LSR

			CLC
			ADC #$60
			STA $0231
			JMP enemy_5_handle_done
		enemy_5_wait_for_respawn:
			LDA enemy_5_respawn_timer
			TAX
			INX
			STX enemy_5_respawn_timer
			LDA #0
			STA enemy_5_spawn_counter
			LDA #$FF
			STA $0231

		enemy_5_handle_done:

	ENEMY_6:
		LDA _enemy_subroutine_counter
		TAX
		INX
		STX _enemy_subroutine_counter

		LDA enemy_6_initialized
		BEQ enemy_6_handle_uninitalized
		LDA enemy_6_active
		BEQ enemy_6_handle_inactive

		; update enemy x and y
		LDA enemy_6_x
		STA $0237
		LDA enemy_6_y
		STA $0234

		; setup input parameters
		LDA enemy_6_x
		STA _projectile_enemy_x
		LDA enemy_6_y
		STA _projectile_enemy_y
		LDA enemy_6_target_x
		STA _projectile_enemy_target_x
		LDA enemy_6_target_y
		STA _projectile_enemy_target_y
	
		JSR PROJECTILE_ENEMY_HANDLER

		; handle function end

		; update positions and targets
		LDA _projectile_enemy_x
		STA enemy_6_x
		LDA _projectile_enemy_y
		STA enemy_6_y
		LDA _projectile_enemy_target_x
		STA enemy_6_target_x
		LDA _projectile_enemy_target_y
		STA enemy_6_target_y

		JMP enemy_6_handle_done
	
		enemy_6_handle_uninitalized:
			LDA _spawn_timer_hi
			CMP #7 ; spawn after 7 cycles
			BNE enemy_6_not_ready_to_initalize
			LDA #1
			STA enemy_6_initialized
			LDA #0
			STA enemy_6_respawn_timer
			LDA _global_spawn_x
			STA enemy_6_x
			STA $0237
			LDA _global_spawn_y
			STA enemy_6_y
			STA $0234
			JSR MOVE_SPAWN_POINT
			enemy_6_not_ready_to_initalize:
			JMP enemy_6_handle_done
		enemy_6_handle_inactive:
			LDA enemy_6_respawn_timer
			BNE enemy_6_wait_for_respawn

			LDA enemy_6_spawn_counter
			CMP #32
			BNE enemy_6_spawn_animation ; need to spawn
			   
			LDA enemy_6_despawn_counter
			BNE enemy_6_despawn_animation ; need to despawn

			LDA enemy_6_respawn_timer
			BNE enemy_6_wait_for_respawn

			; preemptively set the enemy's despawn counter and respawn timer
			LDA #16
			STA enemy_6_despawn_counter

			LDA #$20
			STA $0235

			LDA #1
			STA enemy_6_active
			JMP enemy_6_handle_done
		enemy_6_spawn_animation:
			LDA enemy_6_spawn_counter 
			TAX
			INX
			STX enemy_6_spawn_counter ; increment the spawn counter
			
			LSR 
			LSR
			LSR
			LSR

			CLC
			ADC #$60
			STA $0235
			JMP enemy_6_handle_done
		enemy_6_despawn_animation:
			LDA enemy_6_despawn_counter 
			TAX
			DEX
			CPX #0
			BNE :+
				LDY #100
				STY enemy_6_respawn_timer
				LDA _global_spawn_x
				STA enemy_6_x
				STA $0237 
				LDA _global_spawn_y
				STA enemy_6_y
				STA $0234
				JSR MOVE_SPAWN_POINT
			:
			STX enemy_6_despawn_counter ; increment the spawn counter
			
			LSR 
			LSR
			LSR

			CLC
			ADC #$60
			STA $0235
			JMP enemy_6_handle_done
		enemy_6_wait_for_respawn:
			LDA enemy_6_respawn_timer
			TAX
			INX
			STX enemy_6_respawn_timer
			LDA #0
			STA enemy_6_spawn_counter
			LDA #$FF
			STA $0235
			
		enemy_6_handle_done:

	BONE_MOVEMENT:
	LDA _bone_frame_counter
	TAX
	INX
	STX _bone_frame_counter

	; set the correct animation frame
	LDA _bone_frame_counter
	LSR
	LSR
	AND #$03 ; we only care about the last two bits
	CLC
	ADC #$30 ; we want row 3 of sprites

	STA $0239
	STA $023D
	STA $0241

	LDA $023B
	CLC
	ADC bone_1_vel_x
	STA $023B
	LDA $0238
	CLC
	ADC bone_1_vel_y
	STA $0238

	LDA $023F
	CLC
	ADC bone_2_vel_x
	STA $023F
	LDA $023C
	CLC
	ADC bone_2_vel_y
	STA $023C

	LDA $0243
	CLC
	ADC bone_3_vel_x
	STA $0243
	LDA $0240
	CLC
	ADC bone_3_vel_y
	STA $0240

	;PLAYER
		LDA $0202
		AND #%01000000
		BEQ :+ ; branch if not flipped horizontally 

		; player flipped horizontally 
		LDA player_x
		CLC
		ADC #4
		STA _A_topleft_x ; store player's tile x + 4 to get true sprite x
		CLC 
		ADC #4 ; add four more for the right edge of the hitbox
		STA _A_bottomright_x

		LDA player_y
		STA _A_topleft_y
		CLC
		ADC #8 ; hitbox is 4x8
		STA _A_bottomright_y

		JMP :++

		: ; player not flipped horizontally
		LDA player_x
		STA _A_topleft_x
		CLC 
		ADC #4 ; player hitbox is 4x8
		STA _A_bottomright_x

		LDA player_y
		STA _A_topleft_y
		CLC
		ADC #8 ; hitbox is 4x8
		STA _A_bottomright_y

		:
		;ENEMIES
		LDA enemy_1_x
		STA _B_topleft_x 
		CLC 
		ADC #8 ; hitbox is 8x8
		STA _B_bottomright_x 

		LDA enemy_1_y 
		STA _B_topleft_y 
		CLC 
		ADC #8 ; hitbox is 8x8
		STA _B_bottomright_y

		JSR CHECK_COLLISION
		JSR EXECUTE_PLAYER_COLLISION

		LDA enemy_2_x
		STA _B_topleft_x 
		CLC 
		ADC #8 ; hitbox is 8x8
		STA _B_bottomright_x 

		LDA enemy_2_y 
		STA _B_topleft_y 
		CLC 
		ADC #8 ; hitbox is 8x8
		STA _B_bottomright_y

		JSR CHECK_COLLISION
		JSR EXECUTE_PLAYER_COLLISION

		LDA enemy_3_x
		STA _B_topleft_x 
		CLC 
		ADC #8 ; hitbox is 8x8
		STA _B_bottomright_x 

		LDA enemy_3_y 
		STA _B_topleft_y 
		CLC 
		ADC #8 ; hitbox is 8x8
		STA _B_bottomright_y

		JSR CHECK_COLLISION
		JSR EXECUTE_PLAYER_COLLISION

		LDA enemy_4_x
		STA _B_topleft_x 
		CLC 
		ADC #8 ; hitbox is 8x8
		STA _B_bottomright_x 

		LDA enemy_4_y 
		STA _B_topleft_y 
		CLC 
		ADC #8 ; hitbox is 8x8
		STA _B_bottomright_y

		JSR CHECK_COLLISION
		JSR EXECUTE_PLAYER_COLLISION

		LDA enemy_5_x
		STA _B_topleft_x 
		CLC 
		ADC #8 ; hitbox is 8x8
		STA _B_bottomright_x 

		LDA enemy_5_y 
		STA _B_topleft_y 
		CLC 
		ADC #8 ; hitbox is 8x8
		STA _B_bottomright_y

		JSR CHECK_COLLISION
		JSR EXECUTE_PLAYER_COLLISION

		LDA enemy_6_x
		STA _B_topleft_x 
		CLC 
		ADC #8 ; hitbox is 8x8
		STA _B_bottomright_x 

		LDA enemy_6_y 
		STA _B_topleft_y 
		CLC 
		ADC #8 ; hitbox is 8x8
		STA _B_bottomright_y

		JSR CHECK_COLLISION
		JSR EXECUTE_PLAYER_COLLISION

		;BONES
		LDA $023B
		STA _B_topleft_x 
		CLC 
		ADC #8 ; hitbox is 8x8
		STA _B_bottomright_x 

		LDA $0238
		STA _B_topleft_y 
		CLC 
		ADC #8 ; hitbox is 8x8
		STA _B_bottomright_y

		JSR CHECK_COLLISION
		JSR EXECUTE_PLAYER_COLLISION

		LDA $023F
		STA _B_topleft_x 
		CLC 
		ADC #8 ; hitbox is 8x8
		STA _B_bottomright_x 

		LDA $023C 
		STA _B_topleft_y 
		CLC 
		ADC #8 ; hitbox is 8x8
		STA _B_bottomright_y

		JSR CHECK_COLLISION
		JSR EXECUTE_PLAYER_COLLISION

		LDA $0243
		STA _B_topleft_x 
		CLC 
		ADC #8 ; hitbox is 8x8
		STA _B_bottomright_x 

		LDA $0240 
		STA _B_topleft_y 
		CLC 
		ADC #8 ; hitbox is 8x8
		STA _B_bottomright_y

		JSR CHECK_COLLISION
		JSR EXECUTE_PLAYER_COLLISION

	SPEAR_COLLISION:
		LDA spear_is_active
		BEQ intermediate_jump6

		; setup spear hitbox
		LDA spear_tip_x
		STA _A_topleft_x
		CLC
		ADC #8 
		STA _A_bottomright_x
		LDA spear_tip_y
		STA _A_topleft_y
		CLC
		ADC #16
		STA _A_bottomright_y

		; check collision with enemy 1
		LDA enemy_1_x
		STA _B_topleft_x 
		CLC 
		ADC #8 ; hitbox is 8x8
		STA _B_bottomright_x 
		LDA enemy_1_y 
		STA _B_topleft_y 
		CLC 
		ADC #8 ; hitbox is 8x8
		STA _B_bottomright_y

		JSR CHECK_COLLISION
		BEQ :+ 
		LDA #0
		STA enemy_1_active
		: 

		; check collision with enemy 2
		LDA enemy_2_x
		STA _B_topleft_x 
		CLC 
		ADC #8 ; hitbox is 8x8
		STA _B_bottomright_x 
		LDA enemy_2_y 
		STA _B_topleft_y 
		CLC 
		ADC #8 ; hitbox is 8x8
		STA _B_bottomright_y

		JSR CHECK_COLLISION
		BEQ :+ 
		LDA #0
		STA enemy_2_active
		: 

		; check collision with enemy 3
		LDA enemy_3_x
		STA _B_topleft_x 
		CLC 
		ADC #8 ; hitbox is 8x8
		STA _B_bottomright_x 
		LDA enemy_3_y 
		STA _B_topleft_y 
		CLC 
		ADC #8 ; hitbox is 8x8
		STA _B_bottomright_y

		JSR CHECK_COLLISION
		BEQ :+ 
		LDA #0
		STA enemy_3_active
		: 

		JMP :+
		intermediate_jump6:
		JMP end_spear_collision_check
		:

		; check collision with enemy 4
		LDA enemy_4_x
		STA _B_topleft_x 
		CLC 
		ADC #8 ; hitbox is 8x8
		STA _B_bottomright_x 
		LDA enemy_4_y 
		STA _B_topleft_y 
		CLC 
		ADC #8 ; hitbox is 8x8
		STA _B_bottomright_y

		JSR CHECK_COLLISION
		BEQ :+ 
		LDA #0
		STA enemy_4_active
		: 

		; check collision with enemy 5
		LDA enemy_5_x
		STA _B_topleft_x 
		CLC 
		ADC #8 ; hitbox is 8x8
		STA _B_bottomright_x 
		LDA enemy_5_y 
		STA _B_topleft_y 
		CLC 
		ADC #8 ; hitbox is 8x8
		STA _B_bottomright_y

		JSR CHECK_COLLISION
		BEQ :+ 
		LDA #0
		STA enemy_5_active
		: 

		; check collision with enemy 6
		LDA enemy_6_x
		STA _B_topleft_x 
		CLC 
		ADC #8 ; hitbox is 8x8
		STA _B_bottomright_x 
		LDA enemy_6_y 
		STA _B_topleft_y 
		CLC 
		ADC #8 ; hitbox is 8x8
		STA _B_bottomright_y

		JSR CHECK_COLLISION
		BEQ :+ 
		LDA #0
		STA enemy_6_active
		: 

	end_spear_collision_check:

 	RTI

ENEMY_MOVEMENT_HANDLER:
	; frame skiper
	LDA _enemy_frame_counter
	AND #$01
	CMP #$01
	BEQ intermediate_jump ; skip movement every other frame to make the enemy slower

	; setup targets

	; decide whether to use offset
	; subtract _enemy_x - player_x 
	LDA _enemy_x
	SBC player_x
	STA $00
	BIT $00
	BPL enemy_movement_distance_check_x_skip_negation
	; the difference is negative -- negate it
	EOR #$FF
	SEC   
	ADC #$01
	enemy_movement_distance_check_x_skip_negation:
	; now A has the absolute x distance between the enemy and player
	TAY;LOR SWIFT

	; same for y axis
	LDA _enemy_y
	SBC player_y
	STA $00
	BIT $00
	BPL enemy_movement_distance_check_y_skip_negation
	; the difference is negative -- negate it
	EOR #$FF
	SEC   
	ADC #$01
	enemy_movement_distance_check_y_skip_negation:
	; now A has the absolute y distance between the enemy and player
	STY $00
	ADC $00 
	; now A has x distance + y distance
	LDX player_x
	LDY player_y
	CMP #34
	BCC enemy_dont_use_offset
	TXA
	CLC
	ADC _enemy_offset_x
	TAX
	TYA
	CLC
	ADC _enemy_offset_y
	TAY;LOR SWIFT
	enemy_dont_use_offset:
		STX _enemy_target_x
		STY _enemy_target_y
	
	; intermediate jumping horrendousness 
	JMP skip_intermediate_jump
	intermediate_jump:
	JMP enemy_move_done
	skip_intermediate_jump:

	; horizontal movement
	LDA _enemy_x
	TAX
	CPX _enemy_target_x
	BEQ enemy_end_move_x
	BCC enemy_move_right
	enemy_move_left:
		DEX ; move left

		; make the enemy face left
		LDA _enemy_subroutine_counter
		CMP #1
		BEQ enemy_2_face_left
		CMP #2
		BEQ enemy_3_face_left
		enemy_1_face_left:
			LDA $0222 
			ORA #%01000000
			STA $0222
			JMP enemy_end_face_left
		enemy_2_face_left:
			LDA $0226 
			ORA #%01000000
			STA $0226
			JMP enemy_end_face_left
		enemy_3_face_left:
			LDA $022A 
			ORA #%01000000
			STA $022A
		enemy_end_face_left:

		JMP enemy_end_move_x
	enemy_move_right:
		INX ; move right

		; make the enemy face right
		LDA _enemy_subroutine_counter
		CMP #1
		BEQ enemy_2_face_right
		CMP #2
		BEQ enemy_3_face_right
		enemy_1_face_right:
			LDA $0222 
			AND #%10111111
			STA $0222
			JMP enemy_end_face_right
		enemy_2_face_right:
			LDA $0226 
			AND #%10111111
			STA $0226
			JMP enemy_end_face_right
		enemy_3_face_right:
			LDA $022A 
			AND #%10111111
			STA $022A
		enemy_end_face_right:

	enemy_end_move_x:
		STX _enemy_x

	; vertical movement
	LDA _enemy_y
	TAX
	CPX _enemy_target_y
	BEQ enemy_end_move_y
	BCC enemy_move_down
	enemy_move_up:
		DEX
		JMP enemy_end_move_y
	enemy_move_down:
		INX
	enemy_end_move_y:
		STX _enemy_y

	enemy_move_done:

	; ENEMY 1 ANIMATION
	LDA _enemy_frame_counter

	; slow down animation
	LSR
	LSR
	LSR

	; decide which animation frame to go to
	AND #%00000011 ; look at lower 2 bits
	BEQ enemy_frame_0
	CMP #$03
	BEQ enemy_frame_1
	CMP #$01
	BEQ enemy_frame_1
	JMP enemy_frame_2

	enemy_frame_0:
		LDA #$10 ; pick frame 0
		JMP enemy_animation_done
	enemy_frame_1:
		LDA #$11 ; pick frame 0
		JMP enemy_animation_done
	enemy_frame_2:
		LDA #$12 ; pick frame 0
	enemy_animation_done:
	STA _enemy_sprite_tile_index

	RTS

PROJECTILE_ENEMY_HANDLER:
	LDA _enemy_frame_counter
	AND #$01
	CMP #$01
	BEQ intermediate_jump2 ; skip every other frame of movement
	
	; if the targets are uninitalized (ie, zero), initialize them
	LDA _projectile_enemy_target_x
	BNE skip_target_initialize_x
	JSR PRNG
	STA _projectile_enemy_target_x
	skip_target_initialize_x:
	LDA _projectile_enemy_target_y
	BNE skip_target_initialize_y
	JSR PRNG
	STA _projectile_enemy_target_y
	skip_target_initialize_y:

	; move the enemy towards its target

	; horizontal movement
	LDA _projectile_enemy_x
	TAX
	CPX _projectile_enemy_target_x
	BEQ projectile_enemy_end_move_x
	BCC projectile_enemy_move_right
	projectile_enemy_move_left:
		DEX ; move left

		; make the enemy face left
		LDA _enemy_subroutine_counter
		CMP #4
		BEQ enemy_5_face_left
		CMP #5
		BEQ enemy_6_face_left
		enemy_4_face_left:
			LDA $022E 
			ORA #%01000000
			STA $022E
			JMP projectile_enemy_end_face_left
		enemy_5_face_left:
			LDA $0232 
			ORA #%01000000
			STA $0232
			JMP projectile_enemy_end_face_left
		enemy_6_face_left:
			LDA $0236 
			ORA #%01000000
			STA $0236
		projectile_enemy_end_face_left:

		JMP projectile_enemy_end_move_x
	projectile_enemy_move_right:
		INX ; move right

		; make the enemy face right
		LDA _enemy_subroutine_counter
		CMP #4
		BEQ enemy_5_face_right
		CMP #5
		BEQ enemy_6_face_right
		enemy_4_face_right:
			LDA $022E 
			AND #%10111111
			STA $022E
			JMP projectile_enemy_end_face_right
		enemy_5_face_right:
			LDA $0232 
			AND #%10111111
			STA $0232
			JMP projectile_enemy_end_face_right
		enemy_6_face_right:
			LDA $0236 
			AND #%10111111
			STA $0236
		projectile_enemy_end_face_right:

	projectile_enemy_end_move_x:
		STX _projectile_enemy_x

	JMP skip_intermediate_jump2
	intermediate_jump2:
	JMP projectile_enemy_move_done
	skip_intermediate_jump2:

	; vertical movement
	LDA _projectile_enemy_y
	TAX
	CPX _projectile_enemy_target_y
	BEQ projectile_enemy_end_move_y
	BCC projectile_enemy_move_down
	projectile_enemy_move_up:
		DEX
		JMP projectile_enemy_end_move_y
	projectile_enemy_move_down:
		INX
	projectile_enemy_end_move_y:
		STX _projectile_enemy_y

	; if we've reached the target, pick a new one

	LDA _projectile_enemy_x
	TAX
	CPX _projectile_enemy_target_x
	BNE intermediate_jump3
	LDA _projectile_enemy_y
	TAX
	CPX _projectile_enemy_target_y
	BNE intermediate_jump3

	; when an enemy has to choose a new target, we want to put its bone at its position
	; and we want to set its velocity
	; set its velocity relative to the player
	LDA _projectile_enemy_x
	SBC player_x
	CMP #0
	BEQ bone_vel_x_zero
	AND #%10000000
	CMP #0
	BEQ bone_vel_x_left
	bone_vel_x_right:
		LDA #1
		STA _bone_vel_x
		JMP end_set_bone_vel_x
	bone_vel_x_left:
		LDA #$FF
		STA _bone_vel_x
		JMP end_set_bone_vel_x
	bone_vel_x_zero:
		LDA #0
		STA _bone_vel_x
	end_set_bone_vel_x:

	LDA _projectile_enemy_y
	SBC player_y
	CMP #0
	BEQ bone_vel_y_zero
	AND #%10000000
	CMP #0
	BEQ bone_vel_y_up
	bone_vel_y_down:
		LDA #1
		STA _bone_vel_y
		JMP end_set_bone_vel_y
	bone_vel_y_up:
		LDA #$FF
		STA _bone_vel_y
		JMP end_set_bone_vel_y
	bone_vel_y_zero:
		LDA #0
		STA _bone_vel_y
	end_set_bone_vel_y:
	
	JMP skip_intermediate_jump3
	intermediate_jump3:
	JMP projectile_enemy_skip_pick_new_target
	skip_intermediate_jump3:

	LDA _enemy_subroutine_counter
	CMP #4
	BEQ enemy_5_move_bone
	CMP #5
	BEQ enemy_6_move_bone
	enemy_4_move_bone:
		LDA $022C
		STA $0238 
		LDA $022F
		STA $023B 
		LDA _bone_vel_x
		STA bone_1_vel_x
		LDA _bone_vel_y
		STA bone_1_vel_y
		JMP projectile_enemy_end_move_bone
	enemy_5_move_bone:
		LDA $0230
		STA $023C 
		LDA $0233
		STA $023F 
		LDA _bone_vel_x
		STA bone_2_vel_x
		LDA _bone_vel_y
		STA bone_2_vel_y
		JMP projectile_enemy_end_move_bone
	enemy_6_move_bone:
		LDA $0234
		STA $0240  
		LDA $0237
		STA $0243
		LDA _bone_vel_x
		STA bone_3_vel_x
		LDA _bone_vel_y
		STA bone_3_vel_y
	projectile_enemy_end_move_bone:

	JSR PRNG
	STA _projectile_enemy_target_x
	JSR PRNG
	STA _projectile_enemy_target_y

	projectile_enemy_skip_pick_new_target:
	projectile_enemy_move_done:

	RTS

; SOURCE: https://www.nesdev.org/wiki/Random_number_generator
; Returns a random 8-bit number in A (0-255), clobbers Y (unknown).
; NOTE: This subroutine takes 69 cycles to run --> DONT CALL THIS OFTEN!!
PRNG:
	lda seed+1
	tay ; store copy of high byte
	; compute seed+1 ($39>>1 = %11100)
	lsr ; shift to consume zeroes on left...
	lsr
	lsr
	sta seed+1 ; now recreate the remaining bits in reverse order... %111
	lsr
	eor seed+1
	lsr
	eor seed+1
	eor seed+0 ; recombine with original low byte
	sta seed+1
	; compute seed+0 ($39 = %111001)
	tya ; original high byte
	sta seed+0
	asl
	eor seed+0
	asl
	eor seed+0
	asl
	asl
	asl
	eor seed+0
	sta seed+0
	rts

MOVE_SPAWN_POINT:
	LDA _global_spawn_x
	CLC
	ADC #$30
	BCC no_overflow_spawn_point
	LDA #$1A
	no_overflow_spawn_point:
	STA _global_spawn_x
	RTS

CHECK_COLLISION: 
	; bcc for less than, beq for equal, bcs for greater than
	LDA _A_topleft_x 
	CMP _B_bottomright_x
	BCS NO_COLLISION

	LDA _A_bottomright_x 
	CMP _B_topleft_x
	BCC NO_COLLISION 

	LDA _A_topleft_y 
	CMP _B_bottomright_y
	BCS NO_COLLISION 

	LDA _A_bottomright_y
	CMP _B_topleft_y
	BCC NO_COLLISION

	COLLISION: 
	LDA #1
	RTS
	NO_COLLISION: 
	LDA #0
	RTS 

EXECUTE_PLAYER_COLLISION: 
	BEQ DIDNT_COLLIDE; if accumulator is zero
	STA has_collided 
	LDA $0202
	ORA #%00000011
	STA $0202
	LDA #1
	STA game_over
	DIDNT_COLLIDE: 
	RTS

PALETTEDATA:
	.byte $2E, $27, $17, $15, 	$2E, $20, $07, $3B, 	$2E, $20, $2C, $1C, 	$2E, $05, $00, $20 	;background palettes
	.byte $2E, $05, $00, $20, 	$2E, $20, $07, $3B, 	$2E, $20, $2C, $1C, 	$2E, $20, $05, $15 	;sprite palettes

SPRITEDATA:
;$0200 - The Y coordinate of the sprite on screen
;$0201 - The Tile Index of the sprite from the Pattern Table, allowing you to pick which tile to use for that sprite.
;$0202 - The Attribute Table of the sprite. 
;$0203 - The X coordinate of the sprite on screen
;... continues for the rest of the sprites

; ATTRIBUTE TABLE
;Y, SPRITE NUM, attributes, X
;76543210
;||||||||
;||||||++- Palette (4 to 7) of sprite
;|||+++--- Unimplemented
;||+------ Priority (0: in front of background; 1: behind background)
;|+------- Flip sprite horizontally
;+-------- Flip sprite vertically

	; $0200
	.byte $80, $00, %00000000, $60 ; player
	; $0204
	.byte $00, $FF, %00000001, $00 ; spear_tip
	; $0208
	.byte $00, $FF, %00000001, $00 ; spear_base
	; $020C
	.byte $00, $FF, %00000001, $00 ; spear_swoosh
	; $0210
	.byte $00, $FF, %00100000, $00 ; empty
	; $0214
	.byte $00, $FF, %00100000, $00 ; empty
	; $0218
	.byte $00, $FF, %00100000, $00 ; empty
	; $021C
	.byte $00, $FF, %00100000, $00 ; empty
	; $0220
	.byte $FF, $FF, %00000010, $FF ; enemy 1
	; $0224
	.byte $FF, $FF, %00000010, $FF ; enemy 2
	; $0228
	.byte $FF, $FF, %00000010, $FF ; enemy 3
	; $022C
	.byte $FF, $FF, %00000010, $FF ; enemy 4
	; $0230
	.byte $FF, $FF, %00000010, $FF ; enemy 5
	; $0234
	.byte $FF, $FF, %00000010, $FF ; enemy 6
	; $0238
	.byte $FF, $30, %00000010, $FF ; bone 1
	; $023C
	.byte $FF, $30, %00000010, $FF ; bone 2	
	; $0240
	.byte $FF, $30, %00000010, $FF ; bone 3

	; unused sprites
	.byte $00, $FF, %00100000, $00 ; empty
	.byte $00, $FF, %00100000, $00 ; empty
	.byte $00, $FF, %00100000, $00 ; empty
	.byte $00, $FF, %00100000, $00 ; empty
	.byte $00, $FF, %00100000, $00 ; empty
	.byte $00, $FF, %00100000, $00 ; empty
	.byte $00, $FF, %00100000, $00 ; empty

BACKGROUNDDATA:	;512 BYTES
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $01,$02,$03,$04,$05,$06,$07,$02,$03,$04,$05,$06,$07,$02,$03,$04,$05,$06,$07,$02,$03,$04,$05,$06,$07,$02,$03,$04,$05,$06,$07,$08
	.byte $11,$12,$13,$14,$15,$16,$17,$12,$13,$14,$15,$16,$17,$12,$13,$14,$15,$16,$17,$12,$13,$14,$15,$16,$17,$12,$13,$14,$15,$16,$17,$18
	.byte $21,$22,$23,$24,$25,$26,$27,$22,$23,$24,$25,$26,$27,$22,$23,$24,$25,$26,$27,$22,$23,$24,$25,$26,$27,$22,$23,$24,$25,$26,$27,$28
	.byte $31,$32,$33,$34,$35,$36,$37,$32,$33,$34,$35,$36,$37,$32,$33,$34,$35,$36,$37,$32,$33,$34,$35,$36,$37,$32,$33,$34,$35,$36,$37,$38
	.byte $41,$42,$43,$44,$45,$46,$47,$42,$43,$44,$45,$46,$47,$42,$43,$44,$45,$46,$47,$42,$43,$44,$45,$46,$47,$42,$43,$44,$45,$46,$47,$48
	.byte $51,$52,$53,$54,$55,$56,$57,$52,$53,$54,$55,$56,$57,$52,$53,$54,$55,$56,$57,$52,$53,$54,$55,$56,$57,$52,$53,$54,$55,$56,$57,$58
	.byte $09,$0A,$00,$00,$00,$00,$09,$0A,$00,$00,$00,$00,$09,$0A,$00,$00,$00,$00,$09,$0A,$00,$00,$00,$00,$09,$0A,$00,$00,$00,$00,$09,$0A
	.byte $19,$1A,$00,$00,$00,$00,$19,$1A,$00,$00,$00,$00,$19,$1A,$00,$00,$00,$00,$19,$1A,$00,$00,$00,$00,$19,$1A,$00,$00,$00,$00,$19,$1A
	.byte $29,$2A,$00,$00,$00,$00,$29,$2A,$00,$00,$00,$00,$29,$2A,$00,$00,$00,$00,$29,$2A,$00,$00,$00,$00,$29,$2A,$00,$00,$00,$00,$29,$2A
	.byte $39,$3A,$00,$00,$00,$00,$39,$3A,$00,$00,$00,$00,$39,$3A,$00,$00,$00,$00,$39,$3A,$00,$00,$00,$00,$39,$3A,$00,$00,$00,$00,$39,$3A
	.byte $49,$4A,$00,$00,$00,$00,$49,$4A,$00,$00,$00,$00,$49,$4A,$00,$00,$00,$00,$49,$4A,$00,$00,$00,$00,$49,$4A,$00,$00,$00,$00,$49,$4A
	.byte $59,$5A,$00,$00,$00,$00,$59,$5A,$00,$00,$00,$00,$59,$5A,$00,$00,$00,$00,$59,$5A,$00,$00,$00,$00,$59,$5A,$00,$00,$00,$00,$59,$5A
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

BACKGROUNDPALETTEDATA:	;32 bytes
	.byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	.byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	
INCLUDES:
	.include "wismm.s"
	.include "famitone2.s"
	
.segment "VECTORS"
	.word NMI
	.word RESET
	; specialized hardware interurpts
.segment "CHARS"
	.incbin "rom.chr"
