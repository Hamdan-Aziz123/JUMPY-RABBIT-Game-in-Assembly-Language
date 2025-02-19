; PROJECT OF ATA UR REHMAN anD HAMDAN AZIZ

[org 0x0100]
jmp start1

rows: dw 43            ;rows as a global variable
colLength: dw 264       ;column Full length in word size     
cols: dw 132            ;col as a global variable
fulllength: dw 11352       ;full length of screen
check1: dw 0, 0, 0
countBelowLeft: dw 21
countBelowRight: dw 20
countUpperLeft: dw -20
countUpperRight: dw -21
columnRabbitStart: dw 61
columnRabbitEnd: dw 68
carrotCoordinates: dw 60, 61
originalBoardLocation: dw 55, 75
changingBelowBoardLocation: dw 55, 75
changingupperBoardLocation: dw 55,75
changingRabbitBoardLocation: dw 55,75
colorChange: dw 0
upperBoardColor: dw 0xc7
midBoardColor: dw 0xe7
belowBoardColor: dw 0xa7
score: dw 0
seconds: dw 0
resumeflag: dw 0
terminateFlag: dw 0
startFlag: dw 0
gameOverFlag: dw 0
movementFlag: dw 0
pauseFlag: dw 0
gameOverMessage: db 'GAME OVER...' 
lengthGameOver: dw 12
message: db 'TRY AGAIN... BETTER LUCK NEXT TIME!' ; string to be printed
length1: dw 35                       ; length of the string
message1: db 'SCORE : "  "' ; string to be printed
length2: dw 12                       ; length of the string
message2: db 'PRESS "p" TO PLAY GAME...' ; string to be printed
length3: dw 25                       ; length of the string
message3: db 'PRESS "E" TO EXIT GAME...' ; string to be printed
length4: dw 25                       ; length of the string
message4: db 'PRESS "p" TO START NEW GAME...'
length5: dw 30
message5: db 'YOUR GAME IS PAUSED...'
length6: dw 22
message6: db 'BY HAMDAN AZIZ AND ATA UR REHMAN'                   ; length of the string
length7: dw	32
message7: dw 'YOU QUIT THE GAME...'
length8: dw 20
message8: db 'PRESS ANY KEY TO SAFELY EXIT THE GAME...'
length9: dw 40
message9: db 'PRESS "R" TO RESUME TO GAME...'
length10: dw 30
message10: db 'NOTE: ONCE GAME STARTED, USE "UP-KEY" TO JUMP THE RABBIT!!!'
length11: dw 59

buffer2: times 792 dw 0	
flag: dw 0
flagTimer: dw 0
oldisr: dd 0
oldTimer: dd 0

;--------------------------------------------------------------------
; subroutinfe to create delay on the screen
;--------------------------------------------------------------------
delay:      push cx   
			mov cx, 0xFFff
    loop1:	
        	loop loop1
			mov cx, 0xFFFF
    loop2:		
	        loop loop2
			pop cx
			ret
			
;--------------------------------------------------------------------
; subroutine to clear the screen
;--------------------------------------------------------------------			
clrscr:		push es         ;pushing extra segment on stack
			push ax         ;pushing ax register on stack
			push di         ;pushing di index on stack

			mov ax, 0xb800              ; load video base in ax
			mov es, ax					; point es to video base
			mov di, 0					; point di to top left column

	nextloc:	
	        mov word [es:di], 0x0720	 ;es:di pointint to --> 0xB800:0000 (B8000)
			add di, 2					;move to next screen location
			cmp di, [fulllength]				;comparing di with full length of screen
			jne nextloc					;if no clear next position

			pop di          ;poping di index from stack
			pop ax          ;poping ax register from stack
			pop es          ;poping extra segment from stack
			ret			    ;return back
			
printnum: push bp
mov bp, sp
push es
push ax
push bx
push cx
push dx
push di
mov ax, 0xb800
mov es, ax ; point es to video base
mov ax, [bp+4] ; load number in ax
mov bx, 10 ; use base 10 for division ;1432
mov cx, 0 ; initialize count of digits
nextdigit: mov dx, 0 ; zero upper half of dividend
div bx ; divide by 10            ;quotient in ax , remainder in dx
add dl, 0x30 ; convert digit into ascii value
push dx ; save ascii value on stack
inc cx ; increment count of values
cmp ax, 0 ; is the quotient zero
jnz nextdigit ; if no divide it again
mov al, [cols]				; load al with columns per row
mul byte [bp+8]		; 80 x r
add ax, [bp+6]			; word number (80xr) + c
shl ax, 1				; byte no (((80xr) + c)x2)
mov di, ax               ; point di to required location
nextpos: pop dx ; remove a digit from the stack
mov dh, 0x0f ; use normal attribute
mov [es:di], dx ; print char on screen
add di, 2 ; move to next screen location
loop nextpos ; repeat for all digits on stack
pop di
pop dx
pop cx
pop bx
pop ax 
pop es
pop bp
ret 6					

randomGenerate:

            push bp
			mov bp,sp
			push dx
			
			push cx
			
			;mov ah,0x00
			;int 0x1a
			rdtsc
			
			;mov ax,dx
			xor dx,dx
			mov cx,[bp+4]
			div cx
			add dx,[bp+6]
			mov [bp+8],dx
			
			pop cx
			
			pop dx
			pop bp
			ret 4

printstr:	push bp
			mov bp, sp
			push es
			push ax
			push cx
			push si
			push di

			mov ax, 0xb800
			mov es, ax				; point es to video base

			mov al, [cols]				; load al with columns per row
			mul byte [bp+12]		; 80 x r
			add ax, [bp+10]			; word number (80xr) + c
			shl ax, 1				; byte no (((80xr) + c)x2)
			mov di, ax               ; point di to required location

			
			mov si, [bp+6]			; point si to string
			mov cx, [bp+4]			; load length of string in cx
			mov ah, [bp+8]			; load attribute in ah

nextchar:	mov al, [si]			; load next char of string
			mov [es:di], ax			; show this char on screen
			add di, 2				; move to next screen location
			add si, 1				; move to next char in string
			;call delay
			loop nextchar			; repeat the operation cx times

			pop di
			pop si
			pop cx
			pop ax
			pop es
			pop bp
			ret 10			
;------------------------------------------------------------------
; subroutine to divide the screen 
;--------------------------------------------------------------------			
division:	push bp         ;pushing bp on stack
            mov bp,sp	    ;pointing bp to sp
            push es         ;pushing extra segment on stack
			push ax         ;pushing ax register on stack
			push di         ;pushing di index on stack
            push si         ;pushing si index on stack

			mov ax, 0xb800              ; load video base in ax
			mov es, ax					; point es to video base
			mov di, [bp+6]			    ; point di to desired column
			mov ax,[bp+4]               ;point ax to attribute byte and ascii
            mov si,di                   ;moving si to present di
			add si,[colLength]          ;moving si to end of the row
	nextloc1:	
            mov word [es:di], ax	;es:di pointint to --> 0xB800:0000 (B8000)
			;call delay			    ;delaying in next printing
			add di, 2		        ;move to next screen location
			cmp di,si               ;comparing di with si
			jne nextloc1 			;if no clear next position
			
			
			pop si                  ;poping si index from stack
            pop di                  ;poping di index from stack
			pop ax                  ;poping ax register from stack
			pop es                  ;poping extra segment from stack
			pop bp                  ;poping bp index from stack
			ret 4                   ;return back
			
;--------------------------------------------------------------------
; subroutine to print sky on the screen
;--------------------------------------------------------------------			
			
sky:
            push bp         ;pushing bp on stack
            mov bp,sp	    ;pointing bp to sp
            push es         ;pushing extra segment on stack
			push ax         ;pushing ax register on stack
			push di         ;pushing di index on stack
            push si         ;pushing si index on stack
			push cx         ;pushing cx register on stack
			push bx         ;pushing bx register on stack

			mov ax, 0xb800              ; load video base in ax
			mov es, ax					; point es to video base
			        
			mov di, [bp+8]				; point di to starting location
			mov al,[cols]               ;stroing total columns in al
            mul byte [bp+6]             ;multply with row position
            add ax,[bp+4]               ;add in column position
            shl ax,1			        ;multiply by two the position bcz one cell is of 2 bytes
            mov si, ax                  ;point si to ending location
			
			mov cx, 0                   ;initializing cx with 0 to count rows
   			mov bx, 15                  ;bx to cmpare with cx
			
			mov ah,[bp+10]
			mov al,0x00
    printSky:	
            mov word [es:di], ax	         ;es:di pointint to --> 0xB800:0000 (B8000)
			;call delay	                         ;delaying in next printing
			add di, 2		                     ;move to next screen location
			cmp di,si                            ;comparing di with si
			jne printSky                        ;jmp back if not reached to ending location
			je endSkyFunc 		                ;jmp to next line function if reached to ending location	
			
			
	endSkyFunc:
            	
            pop bx           ;poping bx registerfrom stack		
            pop cx           ;poping cx register from stack
	        pop si            ;poping si index from stack
            pop di           ;poping di index from stack
			pop ax           ;poping ax register from stack
			pop es          ;poping extra segment from stack
			pop bp           ;poping bp index from stack
			ret 8          ;return back
			
;--------------------------------------------------------------------
; subroutine to print bird on the screen
;--------------------------------------------------------------------			

bird:			
            push bp         ;pushing bp on stack
            mov bp,sp	    ;pointing bp to sp
            push es         ;pushing extra segment on stack
			push ax         ;pushing ax register on stack
			push di         ;pushing di index on stack
            push si         ;pushing si index on stack
			
			mov ax, 0xb800           ; load video base in ax
			mov es, ax					; point es to video base
			
            mov al,[cols]         ;stroing total columns in al
            mul byte [bp+6]      ;multply with row position
            add ax,[bp+4]        ;add in column position
            shl ax,1			 ;multiply by two the position bcz one cell is of 2 bytes
			mov di,ax            ;storing ax value in di

	printBird:    
	        ;printing bird.........
			
			
            mov word [es:di], 0xb05e
            add di, 2
			mov word [es:di], 0xb05c
            add di, 2
			mov word [es:di], 0xb02f
            add di, 2
			mov word [es:di], 0xb05e
            call delay
			
			pop si            ;poping si index from stack
            pop di           ;poping di index from stack
			pop ax           ;poping ax register from stack
			pop es          ;poping extra segment from stack
			pop bp           ;poping bp index from stack
			ret 4          ;return back		
			
;--------------------------------------------------------------------
; subroutine to give calls for birds on the screen
;--------------------------------------------------------------------			
	
birds:   
        push ax
		push bx
		mov bx,7
	loopBirds1:
        mov ax, 1         ;giving row position
		push ax
		mov ax, bx        ;giving column poisition
		push ax
		call bird
		add bx,11
		cmp bx,128
		jne loopBirds1
		
		
		
		mov bx, 12
	loopBirds2:	
		mov ax, 2        
		push ax
		mov ax, bx      
		push ax
		call bird
		add bx,15
		cmp bx,132
		jne loopBirds2
		
		
		
		
		mov bx,3
	loopBirds3:	
		mov ax, 3        
		push ax
		mov ax, bx         
		push ax
		call bird
		add bx,27
		cmp bx,111
		jne loopBirds3
		pop bx
		pop ax
		ret                 ;return back
   
;--------------------------------------------------------------------
; subroutine to give calls for mountains on the screen
;--------------------------------------------------------------------	
		
mountains:  
      
		push ax
		
		mprint:
        mov ax, 7    ;giving starting row position
		push ax
		mov ax, 30  ;giving starting column position
		push ax
		call mountain
		
		
		mov ax, 4    ;giving starting row position
	    push ax
		mov ax,55    ;giving starting column position
		push ax
		call mountain
		
		mov ax, 8
		push ax
		mov ax, 77
		push ax
		call mountain
		
		mov ax, 5
		push ax
		mov ax, 96
		push ax
		call mountain
		
	
		
pop ax
        ret		              ;return back

;--------------------------------------------------------------------
; subroutine to print mountain on the screen
;--------------------------------------------------------------------	
			
mountain:   push bp         ;pushing bp on stack
            mov bp,sp	    ;pointing bp to sp
            push es         ;pushing extra segment on stack
			push ax         ;pushing ax register on stack
			push di         ;pushing di index on stack
            push si         ;pushing si index on stack
			push cx         ;pushing cx register on stack

			mov ax, 0xb800            ; load video base in ax
			mov es, ax					; point es to video base
			mov al,[cols]         ;storing total columns
			mul byte [bp+6]       ;total columns x row position
			add ax,[bp+4]        ;(total columns x row position) + column position
			shl ax,1              ;((total columns x row position) + column position) x 2
			mov di, ax					; point di to desired column
			mov ah, 0x6e                ;setting foreground and background
			mov al, '/'               ;setting ascii
			mov si,3696                ;ending loaction 
			mov cx, 0
	loop3:	
            mov word [es:di], ax	; clear next char on screen
			add cx, 2	; move to next screen location
			add di, [colLength]    ;add di with full column length
            ;call delay	   
			cmp di,si         ;until mountain reach  the division location
			ja  endMountain	    ;if end go to end mountain function
			jne leftRightPrint
           
	
	leftRightPrint:	
	        sub di, cx                     ;SUbtracting cx value from di
			shl cx,1                      ;multiply cx with 2 to go upto the right corner
			push si                       ;backup of si in stack
			mov si,0                      ;using si for comparing with cx(where to print)
	l4:   
			mov word [es:di], ax
			add di,2     ;moving to next location
			add si,2     ;incrementing si
			cmp si, cx   ;comparing si with cx
			jna l4      ;if not above , repeating loop
			shr cx,1    ;dividing back by 2 to store previous cx
			add cx,2     ;incrementing cx to increase mountain width
			sub di,cx   ;moving di to mid of mountain again
			pop si     ;restoring si
			jmp loop3  ;again to loop 3		
				
			
           				
    endMountain:   
         	pop cx           ;poping cx register from stack
	        pop si            ;poping si index from stack
            pop di           ;poping di index from stack
			pop ax           ;poping ax register from stack
			pop es          ;poping extra segment from stack
			pop bp           ;poping bp index from stack
			ret 4             ;return back
			
;--------------------------------------------------------------------
; subroutine to print building on the screen
;--------------------------------------------------------------------			
			
building:
            push bp         ;pushing bp on stack
            mov bp,sp	    ;pointing bp to sp
            push es         ;pushing extra segment on stack
			push ax         ;pushing ax register on stack
			push di         ;pushing di index on stack
            push si         ;pushing si index on stack
			push cx         ;pushing cx register on stack
			push bx         ;pushing bx register on stack

			mov ax, 0xb800           ; load video base in ax
			mov es, ax					; point es to video base
			mov al,[cols]           ;storing total columns
			mul byte [bp+12]           ;multply with row position
			add ax,[bp+10]           ;add in column position
			shl ax,1                ;multiply by two the position bcz one cell is of 2 bytes
			mov di, ax				; point di to starting location
			mov al,[cols]        ;storing total columns
            mul byte [bp+8]      ;multply with row position
            add ax,[bp+6]        ;add in column position
            shl ax,1			 ;multiply by two the position bcz one cell is of 2 bytes
            mov si, ax              ;point si to ending location
			mov ah, 0x1f                   ;setting foreground and background
			mov al, '|'               ;setting ascii
			mov cx, [bp+4]              ;point cx to the number of lines count
			mov bx, 0                  ;initializing bx to zero
    nextline:	
            mov word [es:di], ax	         ;es:di pointint to --> 0xB800:0000 (B8000)
			;call delay	              ;delaying in next printing
			add di, 2		     ;move to next screen location
			cmp di,si               ;comparing di with si
			jne nextline         ;jmp back if not reached to ending location
			je nextline1  		;jmp to next line function if reached to ending location	
			
    nextline1:  add bx,1       ;adding 1 in bx
	        cmp bx,cx         ;comparing bx with cx(counter of lines)
			je endfun         ;jmp at the function if equal
            add di,252        ;going to next row by adding 264 and then subtracting 12 to store new line starting location
            mov si, di        ;moving si to di(updating the ending location)
			add si, 12      ; adding 12 to ending location now
            jmp nextline      ;now move back to main function of printing after updating di and si for next line

    endfun:	pop bx           ;poping bx registerfrom stack		
            pop cx           ;poping cx register from stack
	        pop si            ;poping si index from stack
            pop di           ;poping di index from stack
			pop ax           ;poping ax register from stack
			pop es          ;poping extra segment from stack
			pop bp           ;poping bp index from stack
			ret 10           ;return back
			
;--------------------------------------------------------------------
; subroutine to give calls to print building on the screen
;--------------------------------------------------------------------	


buildings:      
push ax
		mov ax,4   ;starting row position
        push ax
		mov ax,0   ;starting col position
		push ax
        mov ax,4   ;endingg row position
        push ax	
		mov ax,6   ;ending col position
		push ax
		mov ax, 10   ;total building length
		push ax
        call building	

        mov ax,6   
        push ax
		mov ax,7
		push ax
        mov ax,6   
        push ax	
		mov ax,13
		push ax
		mov ax, 8
		push ax
        call building
		
		mov ax,9   
        push ax
		mov ax,14
		push ax
        mov ax,9   
        push ax	
		mov ax,20
		push ax
		mov ax, 5
		push ax
        call building
		
		mov ax,5   
        push ax
		mov ax,125
		push ax
        mov ax,5  
        push ax	
		mov ax,131
		push ax
		mov ax, 9
		push ax
        call building
		
		mov ax,7   
        push ax
		mov ax,118
		push ax
        mov ax,7   
        push ax	
		mov ax,124
		push ax
		mov ax, 7
		push ax
        call building
		
		mov ax,8   
        push ax
		mov ax,111
		push ax
        mov ax,8  
        push ax	
		mov ax,117
		push ax
		mov ax, 6
		push ax
        call building
	pop ax	
		ret             ;return back


;--------------------------------------------------------------------
; subroutine to print sea on the screen
;--------------------------------------------------------------------            			
						
sea:       push bp         ;pushing bp on stack
            mov bp,sp	    ;pointing bp to sp
            push es         ;pushing extra segment on stack
			push ax         ;pushing ax register on stack
			push di         ;pushing di index on stack
            push si         ;pushing si index on stack
			push cx         ;pushing cx register on stack
			push bx         ;pushing bx register on stack

			mov ax, 0xb800           ; load video base in ax
			mov es, ax					; point es to video base
			mov al,[cols]           ;storing total columns
			mul byte [bp+10]           ;multply with row position
			add ax,[bp+8]           ;add in column position
			shl ax,1                ;multiply by two the position bcz one cell is of 2 bytes
			mov di, ax				; point di to starting location
			mov al,[cols]           ;storing total columns
			mul byte [bp+6]           ;multply with row position
			add ax,[bp+4]           ;add in column position
			shl ax,1                ;multiply by two the position bcz one cell is of 2 bytes
			mov si, ax				; point si to starting location
			mov ah, 0x1b                ;setting foreground and background
			mov al, '/'                  ;setting ascii
			mov cx, 0             ;point cx to the number of lines count
			mov bx, 15                  ;initializing bx to zero
    nextfuncRoad:	mov cx, 0
	                
	inside:				
            mov word [es:di], ax	         ;es:di pointint to --> 0xB800:0000 (B8000)
			add cx,1
			;call delay	              ;delaying in next printing
            ;call delay			      ;delaying in next printing
			add di, 2		          ;move to next screen location
			cmp di,si                ;comparing si, with di
	        je endroadFunc           ; if equal , function ends
            cmp cx,bx                ;comparing cx, with bx
			jne inside              ;if not equal jmp again to loop1
			je gapNull              ;if equal print some spaces now
			
	gapNull:
	        mov cx,0   ;initialize cx to 0
			gap:  mov word [es:di], 0x1000	         ;es:di pointint to --> 0xB800:0000 (B8000)
			add cx,1                                     ;incrementing cx
			;call delay	              ;delaying in next printing
           
			add di, 2		     ;move to next screen location
			cmp di,si             ;comparing si, with di
	        je endroadFunc        ; if equal , function ends
            cmp cx,bx            ;comparing cx, with bx
			jne gap               ;if not equal jmp again to loop1
			je nextfuncRoad       ;if equal print some slashes now
			

    endroadFunc:	
	        pop bx           ;poping bx registerfrom stack		
            pop cx           ;poping cx register from stack
	        pop si            ;poping si index from stack
            pop di           ;poping di index from stack
			pop ax           ;poping ax register from stack
			pop es          ;poping extra segment from stack
			pop bp           ;poping bp index from stack
			ret 8            ;return back	
			
;--------------------------------------------------------------------
; subroutine to give calls to print boat on the screen
;--------------------------------------------------------------------				

boats:		
push ax
		mov ax,20     
        push ax
        mov ax,10 
        push ax	
		mov ax,20
		push ax
		mov ax,13
		push ax
		call boat	
    
		mov ax,19
        push ax
        mov ax,35   
        push ax	
		mov ax,19
		push ax
		mov ax,38
		push ax
		call boat	
		
		
		mov ax,17
        push ax
        mov ax,70  
        push ax	
		mov ax,17
		push ax
		mov ax,73
		push ax
        call boat	
		
		mov ax,18
        push ax
        mov ax,100 
        push ax	
		mov ax,18
		push ax
		mov ax,103
		push ax
        call boat	
pop ax
		ret
		
;--------------------------------------------------------------------
; subroutine to print boat on the screen
;--------------------------------------------------------------------			
		
boat:
            push bp        
            mov bp,sp	   
            push es        
			push ax         ;pushing ax register on stack
			push di         ;pushing di index on stack
            push si         ;pushing si index on stack
			push bx         ;pushing bx register on stack

			mov ax, 0xb800           ; load video base in ax
			mov es, ax				 ; point es to video base
			mov al,[cols]
			mul byte[bp+10]
			add ax,[bp+8]
			shl ax,1
			mov di, ax			 ; point di to starting location
			mov al,[cols]
			mul byte[bp+6]
			add ax,[bp+4]
			shl ax,1
			mov si,ax
			mov ax,10
			mov bx,0
			mov cx,0
			mov dx,di
			sub dx,260
		
 l1:

	mov word [es:di], 0x7900
	;call delay
	add di,2
	cmp di,si
	jne l1
	je next1
	
next1:

	add di ,264
	sub di,ax
	add si,264
	add si,4
	add bx,1
	add ax,8
	cmp bx,3
	jne l1
	mov di,dx
	
	
l2:
	mov word [es:di], 0x4900
	;call delay
	add di,264
	add bx,1
	cmp bx,7
	jne l2
	sub di,20
	
	mov bx, 0
	mov ax,0
	mov si ,38

l3:
	mov word [es:di], 0x4900
	;call delay
	add di,2
	add cx,2
	cmp cx,si
	jne l3
	je next2

next2:
	mov cx,0
	add di,226
	add bx,4
	add di,bx
	add di,ax
	shl bx,1
	sub si,bx
	shr bx,1
	add ax,8
	cmp bx,12
	jne l3
	je end1

    end1:	
			pop bx           ;poping bx registerfrom stack		
	        pop si            ;poping si index from stack
            pop di           ;poping di index from stack
			pop ax           ;poping ax register from stack
			pop es          ;poping extra segment from stack
			pop bp           ;poping bp in
            ret 8			 ;return back

	
;--------------------------------------------------------------------
; subroutine to give calls to print tree on the screen
;--------------------------------------------------------------------		
	
trees:  
push ax
push bx
        mov bx,25
	loopTrees:
		
        mov ax,12      ;row position
		push ax
		mov ax,bx     ;column position
		push ax
		call tree
		add bx,10
		cmp bx,115
		jne loopTrees
	pop bx
	pop ax
        ret		;return back
			
;--------------------------------------------------------------------
; subroutine print tree on the screen
;--------------------------------------------------------------------				
			
tree:  
            push bp         ;pushing bp on stack
            mov bp,sp	    ;pointing bp to sp
            push es         ;pushing extra segment on stack
			push ax         ;pushing ax register on stack
			push di         ;pushing di index on stack
           

			mov ax, 0xb800           ; load video base in ax
			mov es, ax					; point es to video base
			mov al,[cols]           ;storing total columns
			mul byte [bp+6]           ;multply with row position
			add ax,[bp+4]           ;add in column position
			shl ax,1                ;multiply by two the position bcz one cell is of 2 bytes
			mov di, ax					; point di to starting location
			
	printTree:  		
	         ;printing tree...........

			 
      		mov word [es:di], 0x2700	
			sub di, [colLength]
            mov word [es:di], 0x2700	
			add di, 2
            mov word [es:di], 0x2700		
			add di, 2	
            mov word [es:di], 0x2700		
			add di, [colLength]	
            mov word [es:di], 0x2700		
			sub di, 2
			mov word [es:di], 0x4f7c
			add di, [colLength]	
			mov word [es:di], 0x4f7c
			 

			pop di           ;poping di index from stack
			pop ax           ;poping ax register from stack
			pop es          ;poping extra segment from stack
			pop bp           ;poping bp index from stack
			ret 4            ;return back
			
grass:

            push bp
			mov bp,sp
			push si
			push di
			push es
			push ax
			
			mov ax, 0xb800           ; load video base in ax
			mov es, ax					; point es to video base
			mov al,[cols]
			mul byte[bp+10]
			add ax,[bp+8]
			shl ax,1
			mov di,ax
			mov al,[cols]
			mul byte[bp+6]
			add ax,[bp+4]
			shl ax,1
			mov si,ax
			
			mov ah,0x57
			mov al,' '
			
			
	printGrass:		
            mov[es:di],ax
			add di,2
			cmp di,si
			jne printGrass
			
			
			pop ax
			pop es
			pop di
			pop si
			pop bp
			
			ret 8
			
boards:    
push ax
        mov ax,[upperBoardColor]
		push ax
        mov ax,31
		push ax
		mov ax,[changingupperBoardLocation+0]
		push ax
		mov ax, 31
		push ax
		mov ax, [changingupperBoardLocation+2]
		push ax
	    call board
		
		 mov ax,[midBoardColor]
		push ax
        mov ax,36
		push ax
		mov ax,[changingBelowBoardLocation+0]
		push ax
		mov ax, 36
		push ax
		mov ax, [changingBelowBoardLocation+2]
		push ax
	    call board
		
		mov ax, [belowBoardColor]
		push ax
		mov ax,41
		push ax
		mov ax,[changingRabbitBoardLocation+0]
		push ax
		mov ax, 41
		push ax
		mov ax, [changingRabbitBoardLocation+2]
		push ax
	    call board
     pop ax
        ret	 

board:
            push bp
			mov bp,sp
			push si
			push di
			push es
			push ax
			push cx
			push bx
			
			mov ax, 0xb800           ; load video base in ax
			mov es, ax					; point es to video base
			mov al,[cols]
			mul byte[bp+10]
			add ax,[bp+8]
			shl ax,1
			mov di,ax
			mov al,[cols]
			mul byte[bp+6]
			add ax,[bp+4]
			shl ax,1
			mov si,ax
			mov cx,0
			mov bx,2
			
			 mov ah,[bp+12]
			mov al,' '
	printBoardUpper:		
            mov word[es:di],ax
			add di,2
			cmp di,si
			jne printBoardUpper
			
	endBoardFunc:
            pop bx
            pop cx			
			pop ax
			pop es
			pop di
			pop si
			pop bp
			
			ret 10


carrot:    
            push bp
			mov bp,sp
			push di
			push es
			push ax
			
		
			
			mov ax, 0xb800           ; load video base in ax
			mov es, ax					; point es to video base
			mov al,[cols]
			mul byte[bp+10]
			add ax,[bp+8]
			shl ax,1
			mov di,ax
			
			mov al,[cols]
			mul byte[bp+6]
			add ax,[bp+4]
			shl ax,1
			mov si,ax
			
			mov word[es:di],0x2700
			;add di,2
			mov word[es:si],0x2700
			add di,264
			mov word[es:di],0x4700
			add di,2
			mov word[es:di],0x4700
			
			
            		
			pop ax
			pop es
			pop di
			pop bp
			
			ret 8
scoreBoard:

        		 push bp
			mov bp,sp
			push di
			push es
			push ax
			
		
			
			mov ax, 0xb800           ; load video base in ax
			mov es, ax					; point es to video base
			mov al,[cols]
			mul byte[bp+6]
			add ax,[bp+4]
			shl ax,1
			mov di,ax
			
			
			mov word[es:di],0xf700
			add di,2
			mov word[es:di],0xf700
			add di,2
			mov word[es:di],0xf700
			sub di,2
			mov ah,0x0f
			mov al,[score]
			add al,0x30
			mov [es:di],ax
			
			
			
            		
			pop ax
			pop es
			pop di
			pop bp
			
			ret 4	
			
rabbit:
            push bp         ;pushing bp on stack
            mov bp,sp	    ;pointing bp to sp
            push es         ;pushing extra segment on stack
			push ax         ;pushing ax register on stack
			push di         ;pushing di index on stack
            push si         ;pushing si index on stack
			push cx         ;pushing cx register on stack
			push bx         ;pushing bx register on stack

			mov ax, 0xb800           ; load video base in ax
			mov es, ax					; point es to video base
			mov al,[cols]           ;storing total columns
			mul byte [bp+10]           ;multply with row position
			add ax,[bp+8]           ;add in column position
			shl ax,1                ;multiply by two the position bcz one cell is of 2 bytes
			mov di, ax				; point di to starting location
			mov al,[cols]        ;storing total columns
            mul byte [bp+6]      ;multply with row position
            add ax,[bp+4]        ;add in column position
            shl ax,1			 ;multiply by two the position bcz one cell is of 2 bytes
            mov si, ax              ;point si to ending location
			mov ah, 0xcf                   ;setting foreground and background
			mov al, ' '               ;setting ascii
			mov cx,0
			
	; Ear:	
		; mov bx,di	
		; sub di,268
		; mov word[es:di],0x4f00
		; add di,2
		; mov word[es:di],0x4f00
		; add di,2
		; add di,14
		; mov word[es:di],0x4f00
		; add di,2
		; mov word[es:di],0x4f00
		; add di,2
		; add di,254
		; mov di,bx
		
r1:
	
	mov word[es:di],ax
	;call delay
	add di,2
	cmp di,si
	jne r1
	je r2
	
r2:
	cmp cx,2
	je endfu
	add cx,1
	add di,250
	mov si,di
	add si,14
	jmp r1
	
	
	eyes:
		mov di,bx
		add di,4
		mov word[es:di],0xff00
		add di,4
		mov word[es:di],0xff00
		mov di,bx

    endfu:
		

			pop bx           ;poping bx registerfrom stack		
            pop cx           ;poping cx register from stack
	        pop si            ;poping si index from stack
            pop di           ;poping di index from stack
			pop ax           ;poping ax register from stack
			pop es          ;poping extra segment from stack
			pop bp           ;poping bp index from stack
			ret 8           ;return back
			
;--------------------------------------------------------------------
; subroutine to give calls to print building on the screen
;--------------------------------------------------------------------	

rabbits:      
push ax
		mov ax,38   ;starting row position
        push ax
		mov ax,[columnRabbitStart]   ;starting col position
		push ax
        mov ax,38   ;endingg row position
        push ax	
		mov ax,[columnRabbitEnd]   ;ending col position
		push ax
	
        call rabbit	
	pop ax
ret	
;--------------------------------------------------------------------
; subroutine to show animation on the screen
;--------------------------------------------------------------------				
playAnimation: 
       
        push bx
		push ax
		
		 ; cmp  word[pauseFlag], 1
		 ; je callresume
		 ; jne callresumeAfter
		 ; callresume:
		 
		 ; call resumeScreenPrint
		 ; callresumeAfter:
		mov word[flagTimer],0
		;move leftward the upper sscreen except birds part
		
        mov bx,4     ;storing starting location
		loop12:
		mov ax,1        ;when 1, then rotate on screen
		push ax
		mov ax,131      ;cx
		push ax
		mov ax,1   ;mov 1 to run cld command
		push ax
		mov ax, bx  ;di row
		push ax
		mov ax,0  ;di column
		push ax
		mov ax, bx     ;si row
		push ax
		mov ax,1     ;si column
		push ax
		call moveRow      ;call move Row function moves full row only one position to left
		;call delay
		add bx,1
		cmp bx,14
		jne loop12
		
		;move rightward the lower sscreen
		
		 mov bx ,27   ;storing starting location
		loop14:
		mov ax,1
		push ax
		mov ax,131
		push ax
		mov ax,0    ;mov 0 to run std command
		push ax
		mov ax, bx    ;di row
		push ax
		mov ax,131     ;di column
		push ax
		mov ax, bx    ;si row
		push ax
		mov ax,130   ;si column
		push ax
		call moveRow       ;call move Row function moves full row only one position to right
		;call delay
		sub bx,1
		cmp bx,14
		jne loop14
		
		;move leftward the upper sscreen of birds part
		
		mov bx ,3   ;storing starting location
		loop15:
		mov ax,1
		push ax
		mov ax,131
		push ax
		mov ax,0    ;mov 0 to run std command
		push ax
		mov ax, bx    ;di row
		push ax
		mov ax,131     ;di column
		push ax
		mov ax, bx    ;si row
		push ax
		mov ax,130   ;si column
		push ax
		call moveRow        ;call move Row function moves full row only one position to left
		;call delay
		sub bx,1
		cmp bx,0
		jne loop15
		
		
		
		cmp word[flag],1
		jne stop
		je animationStop
		stop:
		cmp word[belowBoardColor],0x17
		je flagchange
		jne nextAfter
		flagchange:
		mov word[flagTimer],1
		jmp next
		
		nextAfter:
		call belowRabbitboardMov  
		
		next:
		
		cmp word[upperBoardColor],0x17
		je next5
		
		call upperboardMov
		
		next5:
		cmp word[midBoardColor],0x17
		je animationStop
		
		call BelowboardMov
		
		animationStop:
		call delay
		call delay
        pop ax
		pop bx
        ret		       ;return back
		
belowRabbitboardMov:		
       
		push ax
		push bx

        subchck:
		mov ax,[check1+0]
        cmp ax,[countUpperLeft]      ;-30
		jg movLEFT1
		je changeCheck4
        jl addchck
       
		;jb endmov
		
		
		movLEFT1:
		sub word[changingRabbitBoardLocation+0],1
		sub word[changingRabbitBoardLocation+2],1
		
		sub word[columnRabbitStart],1
		sub word[columnRabbitEnd],1
		sub word[check1+0],1
		
		mov bx,37    ;storing starting location
		loopUpprBoardmovLft:
		mov ax,1
		push ax
		mov ax,131
		push ax
		mov ax,1   ;mov 1 to run cld command
		push ax
		mov ax, bx  ;di row
		push ax
		mov ax,0  ;di column
		push ax
		mov ax, bx     ;si row
		push ax
		mov ax,1     ;si column
		push ax
		call moveRow      ;call move Row function moves full row only one position to left
		add bx,1
		
		cmp bx,42
		jne loopUpprBoardmovLft
		
		jmp endUpprBoardMovFunc

        addchck:
	
		mov ax,[check1+0]
        cmp ax,[countUpperRight]        ;-31
		jl movRight1
	    je changeCheck3
		;jg subchck
		
		
		movRight1:
		add word[changingRabbitBoardLocation+0],1
		add word[changingRabbitBoardLocation+2],1
		add word[columnRabbitStart],1
		add word[columnRabbitEnd],1
		add word[check1+0],1
		
		 mov bx,37  ;storing starting location
		loopUpprBoardmov:
		mov ax,1
		push ax
		mov ax,131
		push ax
		mov ax,0   ;mov 0 to run std command
		push ax
		mov ax, bx  ;di row
		push ax
		mov ax,131  ;di column
		push ax
		mov ax, bx     ;si row
		push ax
		mov ax,130     ;si column
		push ax
		call moveRow      ;call move Row function moves full row only one position to left
		
		add bx,1
		cmp bx,42
		jne loopUpprBoardmov
		
		jmp endUpprBoardMovFunc
		
	    changeCheck3:
		mov word[check1+0],20
		jmp subchck
	
		changeCheck4:
		mov word[check1+0],-61
		jmp addchck
		
		
		
		
		endUpprBoardMovFunc:
		pop bx
		pop ax
		;call delay
		ret 
		
upperboardMov:		
       
		push ax
		
		

         subchck1:
		mov ax,[check1+2]
        cmp ax,[countUpperLeft]      ;-30
		jg movLEFT11
		je changeCheck41
        jl addchck1
       
		;jb endmov
		
		
		movLEFT11:
		sub word[changingupperBoardLocation+0],1
		sub word[changingupperBoardLocation+2],1
		sub word[check1+2],1
		
		
		mov ax,1
		push ax
		mov ax,131
		push ax
		mov ax,1   ;mov 1 to run cld command
		push ax
		mov ax, 31  ;di row
		push ax
		mov ax,0  ;di column
		push ax
		mov ax, 31    ;si row
		push ax
		mov ax,1     ;si column
		push ax
		call moveRow      ;call move Row function moves full row only one position to left
		
		jmp endUpprBoardMovFunc1

        addchck1:
	
		mov ax,[check1+2]
        cmp ax,[countUpperRight]        ;-31
		jl movRight11
	    je changeCheck31
		;jg subchck
		
		
		movRight11:
		add word[changingupperBoardLocation+0],1
		add word[changingupperBoardLocation+2],1
		add word[check1+2],1
		
		
		mov ax,1
		push ax
		mov ax,131
		push ax
		mov ax,0   ;mov 0 to run std command
		push ax
		mov ax, 31  ;di row
		push ax
		mov ax,131  ;di column
		push ax
		mov ax, 31    ;si row
		push ax
		mov ax,130     ;si column
		push ax
		call moveRow      ;call move Row function moves full row only one position to left
		
		jmp endUpprBoardMovFunc1
		
	    changeCheck31:
		mov word[check1+2],20
		jmp subchck1
	
		changeCheck41:
		mov word[check1+2],-61
		jmp addchck1
		
		
		
		
		endUpprBoardMovFunc1:
		pop ax
		;call delay
		ret 		

BelowboardMov:  
       push ax
        addCheck:
	
		mov ax,[check1+4]
        cmp ax,[countBelowRight]        ;30
		jl movRight
	    je changeCheck1
		jg subcheck
		
		
		movRight:
		add word[changingBelowBoardLocation+0],1
		add word[changingBelowBoardLocation+2],1
		
		add word[check1+4],1
		
		mov ax,1
		push ax
		mov ax,131
		push ax
		mov ax,0   ;mov 0 to run std command
		push ax
		mov ax, 36  ;di row
		push ax
		mov ax,131  ;di column
		push ax
		mov ax, 36     ;si row
		push ax
		mov ax,130     ;si column
		push ax
		call moveRow      ;call move Row function moves full row only one position to left
	
		
		jmp endBoardMovFunc
		
	    changeCheck1:
		mov word[check1+4],61
		jmp subcheck
		
		
		
	    subcheck:
		mov ax,[check1+4]
        cmp ax,[countBelowLeft]           ;-31
		jg movLEFT
		je changeCheck2
        
       
		;jb endmov
		
		
		movLEFT:
		sub word[changingBelowBoardLocation+0],1
		sub word[changingBelowBoardLocation+2],1
		sub word[check1+4],1
		
		mov ax,1
		push ax
		mov ax,131
		push ax
		mov ax,1   ;mov 1 to run cld command
		push ax
		mov ax, 36  ;di row
		push ax
		mov ax,0  ;di column
		push ax
		mov ax, 36     ;si row
		push ax
		mov ax,1     ;si column
		push ax
		call moveRow      ;call move Row function moves full row only one position to left
		
		
		jmp endBoardMovFunc
		
		
		changeCheck2:
		mov word[check1+4],-20
		jmp addCheck
		
		
		
		
		endBoardMovFunc:
		pop ax
		;call delay
		ret 


;--------------------------------------------------------------------
; subroutine show animation on the screen
;--------------------------------------------------------------------	
moveRow:    
            push bp
			mov bp,sp
			push si
			push di
			push cx
			push ds
			push es
			push ax
			push bx
			push dx
			
            
			mov bx,[bp+12]
			mov cx,[bp+14]
			mov al,[cols]
			mul byte[bp+10]
			add ax,[bp+8]
			shl ax,1
			mov di,ax
			mov al,[cols]
			mul byte[bp+6]
			add ax,[bp+4]
			shl ax,1
			mov si,ax
			
			mov ax, [bp+16]
			cmp ax,1
			je screen
			
			screen:
			mov ax,0xb800
			mov es, ax ; point es to video base
			mov ds, ax ; point ds to video base
			
			rotate:
			mov dx,[es:di]
			
	        cmp bx,1
			je cldrun
			jne stdrun
			
			cldrun:
			cld       ; df=0, add di,2, add si,2
			rep movsw   ;mov [es:di],[ds:si].... add di,2...add si,2.....dec cx(until cx=0)
			jmp endMovFunc
		
			stdrun:
			std      ; df=1, sub di,2, sub si,2
			rep movsw      ;mov [es:di],[ds:si].... sub di,2...sub si,2.....dec cx(until cx=0)
			
			
	endMovFunc:		
			mov [es:di],dx
			
            pop dx
    		pop bx
			pop ax
			pop es
			pop ds
			pop cx
			pop di
			pop si
			pop bp
			
			ret	14
			
newBoard:
push dx
push bx
push ax
            mov ax,0xcccc
			push ax
			mov ax,0
			push ax
			mov ax,3
			push ax
			call randomGenerate
			pop dx
			
			cmp dx,0
			jne nextGenCheck1
			mov bx,0x17
			mov [upperBoardColor],bx
			jmp generateBoardLocation
			
			nextGenCheck1:
			cmp dx,1
			jne nextGenCheck2
			mov bx,0xe7
			mov [upperBoardColor],bx
			jmp generateBoardLocation
			
			nextGenCheck2:
			cmp dx,2
			jne generateBoardLocation
			mov bx,0xc7
			mov [upperBoardColor],bx
			
			
			
			generateBoardLocation:
			
			; mov ax,0xcccc
			; push ax
			; mov ax,55
			; push ax
			; mov ax,21
			; push ax
            ; call randomGenerate	
            ; pop dx

            ; mov [changingupperBoardLocation+0],dx
			; add dx,20
			; mov [changingupperBoardLocation+2],dx
			
			
			movPrintBoad:
			mov ax,[upperBoardColor]
		    push ax
            mov ax,31
		    push ax
		    mov ax,[changingupperBoardLocation+0]
		    push ax
		    mov ax, 31
		    push ax
		    mov ax, [changingupperBoardLocation+2]
		    push ax
	        call board
			
			
			
pop ax
pop bx
pop dx
        ret			
			
box:
            push bp         ;pushing bp on stack
            mov bp,sp	    ;pointing bp to sp
            push es         ;pushing extra segment on stack
			push ax         ;pushing ax register on stack
			push di         ;pushing di index on stack
            push si         ;pushing si index on stack
			push cx         ;pushing cx register on stack
			push bx         ;pushing bx register on stack

			mov ax, 0xb800           ; load video base in ax
			mov es, ax					; point es to video base
			mov al,[cols]           ;storing total columns
			mul byte [bp+12]           ;multply with row position
			add ax,[bp+10]           ;add in column position
			shl ax,1                ;multiply by two the position bcz one cell is of 2 bytes
			mov di, ax				; point di to starting location
			mov al,[cols]        ;storing total columns
            mul byte [bp+8]      ;multply with row position
            add ax,[bp+6]        ;add in column position
            shl ax,1			 ;multiply by two the position bcz one cell is of 2 bytes
            mov si, ax              ;point si to ending location
			mov ah, 0x1f                   ;setting foreground and background
			mov al, ' '               ;setting ascii
			mov cx, [bp+4]              ;point cx to the number of lines count
			mov bx, 0                  ;initializing bx to zero
    nextlineBox:	
            mov word [es:di], ax	         ;es:di pointint to --> 0xB800:0000 (B8000)
			;call delay	              ;delaying in next printing
			add di, 2		     ;move to next screen location
			cmp di,si               ;comparing di with si
			jne nextlineBox         ;jmp back if not reached to ending location
			je nextline1Box  		;jmp to next line function if reached to ending location	
			
    nextline1Box:  add bx,1       ;adding 1 in bx
	        cmp bx,cx         ;comparing bx with cx(counter of lines)
			je endfunBox         ;jmp at the function if equal
            add di,[bp+16]       ;going to next row by adding 264 and then subtracting 12 to store new line starting location
            mov si, di        ;moving si to di(updating the ending location)
			add si, [bp+14]     ; adding 12 to ending location now
            jmp nextlineBox      ;now move back to main function of printing after updating di and si for next line

    endfunBox:
	     	pop bx           ;poping bx registerfrom stack		
            pop cx           ;poping cx register from stack
	        pop si            ;poping si index from stack
            pop di           ;poping di index from stack
			pop ax           ;poping ax register from stack
			pop es          ;poping extra segment from stack
			pop bp           ;poping bp index from stack
			ret 14          ;return back

		  
Rabbit: 
    push bp    
    mov bp,sp
    push es
    push ax
    push di
    push si
	push cx
   
    mov ax,0xb800
    mov es,ax
    mov di,[bp+4]
	push di
   
    mov ax,0xff20
    mov si,di
    add si,16
	mov cx,3
	
NextLocation1:  
          cmp cx,0
		  je endRab
          mov word[es:di],ax
          add di,2
          cmp di,si
          jne NextLocation1			  
          dec cx
		  add di,264
		  sub di,16
		  add si,264
		  jmp NextLocation1
		  
endRab:	  pop di
		  sub di,268
		  mov word[es:di],ax
		  add di,2
		  mov word[es:di],ax
		  add di,18
		  mov word[es:di],ax
		  add di,2
		  mov word[es:di],ax
		  mov ah,0x08
		  mov al,0x5e
		  add di,514
		  mov word[es:di],ax
		  add di,6
		  mov word[es:di],ax
     		  
		  pop cx
          pop si
          pop di
          pop ax
          pop es
          pop bp
          ret 2	 	


quitGame:

push ax
skyprintQuitGame:
			;jmp lopInt16			
push 0x80
push 0
push 42
push 132
call sky
			
		
		push 212	
		mov ax,52
        push ax		
		mov ax,21   ;starting row position
        push ax
		mov ax,52   ;starting col position
		push ax
        mov ax,21   ;endingg row position
        push ax	
		mov ax,78  ;ending col position
		push ax
		mov ax, 3   ;total building length
		push ax
        call box	
		
		mov ax, 22
		push ax				; push r position............[bp+12]
		mov ax, 55
		push ax				; push c position............[bp+10]
		mov ax, 0x1f			; blue on black attribute
		push ax				; push attribute............[bp+8]
		mov ax, message7
		push ax				; push address of message............[bp+6]
		push word [length8]	; push message length ....[bp+4]
		call printstr ; call the printstr subroutine
		
		
		push 174	
		mov ax,90
        push ax		
		mov ax,26   ;starting row position
        push ax
		mov ax,45   ;starting col position
		push ax
        mov ax,26   ;endingg row position
        push ax	
		mov ax,90  ;ending col position
		push ax
		mov ax, 3   ;total building length
		push ax
        call box	
		
		mov ax, 27
		push ax				; push r position............[bp+12]
		mov ax, 48
		push ax				; push c position............[bp+10]
		mov ax, 0x1f			; blue on black attribute
		push ax				; push attribute............[bp+8]
		mov ax, message8
		push ax				; push address of message............[bp+6]
		push word [length9]	; push message length ....[bp+4]
		call printstr ; call the printstr subroutine
		
		
		
		
		mov ax,39
		push ax
		mov ax,0
		push ax
		mov ax, 43
		push ax
		mov ax, 132
		push ax
	    call grass
		
		push 9718
		call Rabbit
		
		mov ah,0x04
		int 0x16
		
		
		mov ah,00h
		int 16h
		
		; cmp al,'z'
		; je quitt
		
		; jmp endquitGameFunc
		
		; quitt:
		
		; mov word[quitGameFlag],1
		
endquitGameFunc:
pop ax
ret		
			
gameOver:
push ax



call clrscr

push 0x80
push 0
push 42
push 132
call sky


call gameOverPrint
        
		
lopInt16GameOver:
		
		; mov ah, 0x0e
		; mov al, 'x'
		; int 0x10
		;mov ah,0x04       
		;int 0x16
		mov ah,0x04       
		int 0x16
		
		
		mov ah,00h
		int 0x16
		
		; mov ah, 0x0e
		; int 0x10
		cmp al, 'p' ;0x1b
		je pressPgamOvr
		jne nextCheckESC
		
		pressPgamOvr:
		mov word[startFlag], 1
		jmp endGameOverFunc
		
		nextCheckESC:
		cmp al, 'e'
		je pressESCgamOvr
		jne lopInt16GameOver
		
		pressESCgamOvr:
		mov word[terminateFlag],1
		
endGameOverFunc:		
pop ax
ret	


			
	
gameOverPrint:

           
			
			push ax
			jmp gameOverPrintContinue
			
		gameOverPrintContinue:
		push 228	
		mov ax,36
        push ax		
		mov ax,6   ;starting row position
        push ax
		mov ax,53   ;starting col position
		push ax
        mov ax,6   ;endingg row position
        push ax	
		mov ax,71  ;ending col position
		push ax
		mov ax, 3   ;total building length
		push ax
        call box	
		
			mov ax, 7
		push ax				; push r position............[bp+12]
		mov ax, 56
		push ax				; push c position............[bp+10]
		mov ax, 0x1f			; blue on black attribute
		push ax				; push attribute............[bp+8]
		mov ax, gameOverMessage
		push ax				; push address of message............[bp+6]
		push word [lengthGameOver]	; push message length ....[bp+4]
		call printstr ; call the printstr subroutine
			
		push 180	
		mov ax,84
        push ax		
		mov ax,12   ;starting row position
        push ax
		mov ax,40   ;starting col position
		push ax
        mov ax,12   ;endingg row position
        push ax	
		mov ax,82  ;ending col position
		push ax
		mov ax, 3   ;total building length
		push ax
        call box	
			
			
			
		mov ax, 13
		push ax				; push r position............[bp+12]
		mov ax, 44
		push ax				; push c position............[bp+10]
		mov ax, 0x1f			; blue on black attribute
		push ax				; push attribute............[bp+8]
		mov ax, message
		push ax				; push address of message............[bp+6]
		push word [length1]	; push message length ....[bp+4]
		call printstr ; call the printstr subroutine
		
		
		push 228	
		mov ax,36
        push ax		
		mov ax,21   ;starting row position
        push ax
		mov ax,52   ;starting col position
		push ax
        mov ax,21   ;endingg row position
        push ax	
		mov ax,70  ;ending col position
		push ax
		mov ax, 3   ;total building length
		push ax
        call box	
		
		mov ax, 22
		push ax				; push r position............[bp+12]
		mov ax, 55
		push ax				; push c position............[bp+10]
		mov ax, 0x1f			; blue on black attribute
		push ax				; push attribute............[bp+8]
		mov ax, message1
		push ax				; push address of message............[bp+6]
		push word [length2]	; push message length ....[bp+4]
		call printstr ; call the printstr subroutine
		
		push 22
		push 64
	    push word[score]
		call printnum
		
		push 180	
		mov ax,84
        push ax		
		mov ax,29   ;starting row position
        push ax
		mov ax,40   ;starting col position
		push ax
        mov ax,29   ;endingg row position
        push ax	
		mov ax,82  ;ending col position
		push ax
		mov ax, 3   ;total building length
		push ax
        call box	
		
		
		mov ax, 30
		push ax				; push r position............[bp+12]
		mov ax, 48
		push ax				; push c position............[bp+10]
		mov ax, 0x1f			; blue on black attribute
		push ax				; push attribute............[bp+8]
		mov ax, message4
		push ax				; push address of message............[bp+6]
		push word [length5]	; push message length ....[bp+4]
		call printstr ; call the printstr subroutine
		
		
		push 180	
		mov ax,84
        push ax		
		mov ax,35   ;starting row position
        push ax
		mov ax,40   ;starting col position
		push ax
        mov ax,35   ;endingg row position
        push ax	
		mov ax,82  ;ending col position
		push ax
		mov ax, 3   ;total building length
		push ax
        call box	
		
		mov ax, 36
		push ax				; push r position............[bp+12]
		mov ax, 48
		push ax				; push c position............[bp+10]
		mov ax, 0x1f			; blue on black attribute
		push ax				; push attribute............[bp+8]
		mov ax, message3
		push ax				; push address of message............[bp+6]
		push word [length4]	; push message length ....[bp+4]
		call printstr ; call the printstr subroutine
		
		; mov ax,0x27
		; push ax
		mov ax,39
		push ax
		mov ax,0
		push ax
		mov ax, 43
		push ax
		mov ax, 132
		push ax
	    call grass
		
		push 9718
		call Rabbit
            
			pop ax
			
			
			
			ret	
			
resumeScreenPrint:

			
			push ax
skyprint:
			;jmp lopInt16			
push 0x90
push 0
push 42
push 132
call sky
			
		push 180	
		mov ax,84
        push ax		
		mov ax,7   ;starting row position
        push ax
		mov ax,40   ;starting col position
		push ax
        mov ax,7   ;endingg row position
        push ax	
		mov ax,82  ;ending col position
		push ax
		mov ax, 3   ;total building length
		push ax
        call box	
			
			
			
		mov ax, 8
		push ax				; push r position............[bp+12]
		mov ax, 50
		push ax				; push c position............[bp+10]
		mov ax, 0x1f			; blue on black attribute
		push ax				; push attribute............[bp+8]
		mov ax, message5
		push ax				; push address of message............[bp+6]
		push word [length6]	; push message length ....[bp+4]
		call printstr ; call the printstr subroutine
		
		
		push 228	
		mov ax,36
        push ax		
		mov ax,15   ;starting row position
        push ax
		mov ax,52   ;starting col position
		push ax
        mov ax,15   ;endingg row position
        push ax	
		mov ax,70  ;ending col position
		push ax
		mov ax, 3   ;total building length
		push ax
        call box	
		
		mov ax, 16
		push ax				; push r position............[bp+12]
		mov ax, 55
		push ax				; push c position............[bp+10]
		mov ax, 0x1f			; blue on black attribute
		push ax				; push attribute............[bp+8]
		mov ax, message1
		push ax				; push address of message............[bp+6]
		push word [length2]	; push message length ....[bp+4]
		call printstr ; call the printstr subroutine
		
		push 16
		push 64
	    push word[score]
		call printnum
		
		push 180	
		mov ax,84
        push ax		
		mov ax,24   ;starting row position
        push ax
		mov ax,40   ;starting col position
		push ax
        mov ax,24   ;endingg row position
        push ax	
		mov ax,82  ;ending col position
		push ax
		mov ax, 3   ;total building length
		push ax
        call box	
		
		
		mov ax, 25
		push ax				; push r position............[bp+12]
		mov ax, 48
		push ax				; push c position............[bp+10]
		mov ax, 0x1f			; blue on black attribute
		push ax				; push attribute............[bp+8]
		mov ax, message9
		push ax				; push address of message............[bp+6]
		push word [length10]	; push message length ....[bp+4]
		call printstr ; call the printstr subroutine
		
		push 180	
		mov ax,84
        push ax		
		mov ax,29   ;starting row position
        push ax
		mov ax,40   ;starting col position
		push ax
        mov ax,29   ;endingg row position
        push ax	
		mov ax,82  ;ending col position
		push ax
		mov ax, 3   ;total building length
		push ax
        call box	
		
		
		mov ax, 30
		push ax				; push r position............[bp+12]
		mov ax, 48
		push ax				; push c position............[bp+10]
		mov ax, 0x1f			; blue on black attribute
		push ax				; push attribute............[bp+8]
		mov ax, message4
		push ax				; push address of message............[bp+6]
		push word [length5]	; push message length ....[bp+4]
		call printstr ; call the printstr subroutine
		
		
		push 180	
		mov ax,84
        push ax		
		mov ax,34   ;starting row position
        push ax
		mov ax,40   ;starting col position
		push ax
        mov ax,34   ;endingg row position
        push ax	
		mov ax,82  ;ending col position
		push ax
		mov ax, 3   ;total building length
		push ax
        call box	
		
		mov ax, 35
		push ax				; push r position............[bp+12]
		mov ax, 48
		push ax				; push c position............[bp+10]
		mov ax, 0x1f			; blue on black attribute
		push ax				; push attribute............[bp+8]
		mov ax, message3
		push ax				; push address of message............[bp+6]
		push word [length4]	; push message length ....[bp+4]
		call printstr ; call the printstr subroutine
		
		; mov ax,0xa7
		; push ax
		mov ax,39
		push ax
		mov ax,0
		push ax
		mov ax, 43
		push ax
		mov ax, 132
		push ax
	    call grass
		
		push 9718
		call Rabbit
		
		
		
		
		lopInt16:
		
		mov ah,0x04
		int 0x16
		
		mov ah,00h
		int 16h
		
		; mov ah, 0x0e
		; int 0x10
		
		cmp al, 'p' ;0x1b
		je pressP
		jne nextPress
		
		pressP:
		mov word[startFlag], 1
		jmp endResumeFunc
		
		nextPress:
		cmp al, 'e'
		je pressESC
		jne nextPressAgain
		pressESC:
		mov word[terminateFlag],1
		jmp endResumeFunc
		
		nextPressAgain:
		cmp al,'r'
		je pressR
		jne lopInt16
		pressR:
		mov word[resumeflag],1
		
		
		endResumeFunc:
		pop ax
		ret	
		
		

Print_JumpyRabbit:

push ax
		
		mov ax,0xb0
		push ax
		mov ax,0
		push ax
		mov ax,42
		push ax
		mov ax,264
		push ax
		call sky
		
	

		mov ax, 5
		push ax
		mov ax, 96
		push ax
		call mountain
					mov ax,7   
        push ax
		mov ax,118
		push ax
        mov ax,7   
        push ax	
		mov ax,124
		push ax
		mov ax, 7
		push ax
        call building
				mov ax,5   
        push ax
		mov ax,125
		push ax
        mov ax,5  
        push ax	
		mov ax,131
		push ax
		mov ax, 9
		push ax
        call building
		
		mov ax,8   
        push ax
		mov ax,111
		push ax
        mov ax,8  
        push ax	
		mov ax,117
		push ax
		mov ax, 6
		push ax
        call building 
			
		push 202	
		mov ax,62
        push ax		
		mov ax,25   ;starting row position
        push ax
		mov ax,50   ;starting col position
		push ax
        mov ax,25   ;endingg row position
        push ax	
		mov ax,81  ;ending col position
		push ax
		mov ax, 3   ;total building length
		push ax
        call box
		
		
		push 202	
		mov ax,62
        push ax		
		mov ax,31  ;starting row position
        push ax
		mov ax,50   ;starting col position
		push ax
        mov ax,31   ;endingg row position
        push ax	
		mov ax,81  ;ending col position
		push ax
		mov ax, 3   ;total building length
		push ax
        call box
		
	
		
		mov ax,39
		push ax
		mov ax,0
		push ax
		mov ax, 43
		push ax
		mov ax, 132
		push ax
	    call grass
		
		push 9518
		call Rabbit
		
		mov ax, 26
		push ax				; push r position............[bp+12]
		mov ax, 54
		push ax			; push c position............[bp+10]
		mov ax, 0x1f			; blue on black attribute
		push ax				; push attribute............[bp+8]
		mov ax, message2
		push ax				; push address of message............[bp+6]
		push word [length3]	; push message length ....[bp+4]
		call printstr ; call the printstr subroutine
		
			mov ax, 32
		push ax				; push r position............[bp+12]
		mov ax, 54
		push ax			; push c position............[bp+10]
		mov ax, 0x1f			; blue on black attribute
		push ax				; push attribute............[bp+8]
		mov ax, message3
		push ax				; push address of message............[bp+6]
		push word [length4]	; push message length ....[bp+4]
		call printstr ; call the printstr subroutine
				
	mov ax, 40
		push ax				; push r position............[bp+12]
		mov ax, 90
		push ax				; push c position............[bp+10]
		mov ax, 0x1f			; blue on black attribute
		push ax				; push attribute............[bp+8]
		mov ax, message6
		push ax				; push address of message............[bp+6]
		push word [length7]	; push message length ....[bp+4]
		call printstr ; call the printstr subroutine	

		push 130	
		mov ax,134
        push ax		
		mov ax,18  ;starting row position
        push ax
		mov ax,35  ;starting col position
		push ax
        mov ax,18   ;endingg row position
        push ax	
		mov ax,102  ;ending col position
		push ax
		mov ax, 3   ;total building length
		push ax
        call box
		
		mov ax, 19
		push ax				; push r position............[bp+12]
		mov ax, 39
		push ax				; push c position............[bp+10]
		mov ax, 0x1f			; blue on black attribute
		push ax				; push attribute............[bp+8]
		mov ax, message10
		push ax				; push address of message............[bp+6]
		push word [length11]	; push message length ....[bp+4]
		call printstr ; call the printstr subroutine
							
      
		mov bx,7
	loopBirds11:
        mov ax, 1         ;giving row position
		push ax
		mov ax, bx        ;giving column poisition
		push ax
		call bird
		add bx,11
		cmp bx,128
		jne loopBirds11
		
		
		
		mov bx, 12
	loopBirds12:	
		mov ax, 2        
		push ax
		mov ax, bx      
		push ax
		call bird
		add bx,15
		cmp bx,132
		jne loopBirds12
		
		
		
		mov ax,[rows]               ;storing rows in ax
		mov bx, [colLength]            ;storing columns in bx
		mul bx                   ;multiply ax into bx and storing answer in ax
		sub ax, [colLength]            ;subtracting one column from total length as it is odd
		mov bx,3                 ;moving 3 in bx register
		div bx                   ;dividing 1/3rd the whole screen
		push ax                  ;pushing ax
		mov bx, 0x7300          ;pushing attribute and ascii
		push bx
		call division
push es
push ax
push bx
push cx
push dx
push si
push di
push bp
   mov ax, 0xb800  ;video memory
   mov es, ax
   mov al, " "                   ; storing ASCII value of '' to dl
   mov ah, 0x11   
mov cx,0   ; storing attribute value of '_' to dh
  ; mov di, 484                  ;746
    ; Top line
    mov di, 1594
;--------------J-------------
j1:
	call Large_delay
	mov [es:di], ax
	add di,2
	
	cmp di,1604
	jne j1

sub di,2
add di,264
;add di,160
j2:
call Large_delay
	add cx,1
	mov [es:di], ax
	;add di,160
	add di,264
	cmp cx,3
	jne j2
	
sub di,8
;sub di, 160
sub di,264
mov [es:di], ax
;add di ,160
add di ,264
j3:
call Large_delay
mov [es:di], ax
	add di,2
	add cx,1
	cmp cx,8
	jne j3
   
;------------- U --------------
   mov di,1606
ll4:
   call Large_delay
   mov [es:di], ax
   add di,264
   cmp di,2926
   jne ll4
call Large_delay
   mov di,2664
   mov [es:di], ax
   mov di,2666
   mov [es:di], ax
   mov di,2668
l5:
  call Large_delay
   mov [es:di], ax
   sub di,264
   cmp di,1348
   jne l5
   
   ;------- M ------------
mov di,1616
l12:
call Large_delay
  mov [es:di], ax
  add di,264
  cmp di,2936
  jne l12
  call Large_delay
mov di,1618
mov [es:di], ax
mov di,1620;1192
mov [es:di], ax
mov di,1622;1196
mov [es:di], ax
mov di,1634;1196
mov [es:di], ax
mov di,1636;1196
mov [es:di], ax
mov di,1624;1198
mov [es:di], ax
mov di,1626;1196
mov [es:di], ax
mov di,1628;1196
mov [es:di], ax
l13:
call Large_delay
   mov [es:di], ax
   add di,264
   cmp di,2948 
   jne l13
mov di,1622;1194
l14:
call Large_delay
   mov [es:di], ax
   add di,264
   cmp di,2942
   jne l14
;-------- P ----------------
   mov di,1634
l6:
call Large_delay
   mov [es:di], ax
   add di,264
   cmp di,2954
   jne l6
   call Large_delay
   mov di,1636
   mov [es:di], ax
   mov di,1638
   mov [es:di], ax
   mov di,1640
   mov [es:di], ax
   mov di,1642
l7:
 call Large_delay
   mov [es:di], ax
   add di,264
   cmp di,2434
   jne l7
   call Large_delay
mov di,2164
mov [es:di], ax
mov di,2166
mov [es:di], ax
mov di,2168
mov [es:di], ax

; ;---------Y---------
mov di,1646
y1:
call Large_delay
   mov [es:di], ax
add di ,2

   add di,262
   add cx,1
   cmp cx,10
   
   jne y1
  mov di, 1654
 y2:
call Large_delay
   mov [es:di], ax
add di ,2


   add di,262
   add cx,1
   cmp cx,12
   
	jne y2

;add di,150
sub di,6
y3:
call Large_delay
   mov [es:di], ax
  add di,2

   add cx,1
   cmp cx,15
	jne y3
	
add di,260
y4:
call Large_delay
   mov [es:di], ax
  add di,2
  
add di,262
   add cx,1
   cmp cx,17
	jne y4
; ;------- R ------------
mov di, 1668  ; Adjusting the initial di value for a 43x132 resolution

l9:
    call Large_delay
    mov [es:di], ax
    add di, 264  ; Adjusting di value
    cmp di, 2988  ; Adjusting comparison value
    jne l9

    call Large_delay
    mov di, 1670  ; Adjusting di value
    mov [es:di], ax
    mov di, 1672  ; Adjusting di value
    mov [es:di], ax
    mov di, 1674  ; Adjusting di value
    mov [es:di], ax
    mov di, 1676  ; Adjusting di value
    mov [es:di], ax
    mov di, 1678  ; Adjusting di value
    mov [es:di], ax
    mov di, 1942  ; Adjusting di value
    mov [es:di], ax
    mov di, 2206  ; Adjusting di value
    mov [es:di], ax

l10:
    call Large_delay
    mov [es:di], ax
    sub di, 2; Adjusting di value
    cmp di, 2196  ; Adjusting comparison value
    jne l10 

    mov di, 2204  ; Adjusting di value
l11:
    call Large_delay
    mov [es:di], ax
    add di, 264 ; Adjusting di value
    cmp di, 2996  ; Adjusting comparison value
    jne l11 

    call Large_delay
    mov di, 2734  ; Adjusting di value
    mov [es:di], ax


;------- A ------------------
mov di, 1682  ; Adjusted di value
l15:
call Large_delay
  mov [es:di], ax
  add di, 264  ; Adjusting di value
  cmp di, 3002  ; Adjusting comparison value
  jne l15

call Large_delay
mov di, 1684  ; Adjusted di value
mov [es:di], ax
mov di, 1686  ; Adjusted di value
mov [es:di], ax
mov di, 1688  ; Adjusted di value
mov [es:di], ax
mov di, 1690  ; Adjusted di value
l16:
call Large_delay
  mov [es:di], ax
  add di, 264  ; Adjusting di value
  cmp di, 3010  ; Adjusting comparison value
  jne l16

call Large_delay
mov di, 2212  ; Adjusted di value
mov [es:di], ax
mov di, 2214  ; Adjusted di value
mov [es:di], ax
mov di, 2216  ; Adjusted di value
mov [es:di], ax

;--------B--------
mov di, 1694  ; Adjusted di value
b0:
call Large_delay
  mov [es:di], ax
  add di, 264  ; Adjusting di value
  cmp di, 3014  ; Adjusting comparison value
  jne b0

mov di, 1694  ; Adjusted di value
b1:
call Large_delay
  mov [es:di], ax
  add di, 2  ; Adjusting di value
  cmp di, 1704  ; Adjusting comparison value
  jne b1
 
 mov di, 2222  ; Adjusted di value
b2:
call Large_delay
  mov [es:di], ax
  add di, 2  ; Adjusting di value
  cmp di, 2232  ; Adjusting comparison value
  jne b2
  
 mov di, 2750 ; Adjusted di value
 b3:
 call Large_delay
  mov [es:di], ax
  add di, 2  ; Adjusting di value
  cmp di, 2760 ; Adjusting comparison value
  jne b3
 
 mov di, 1968  ; Adjusted di value
   mov [es:di], ax
   
   mov di, 2496  ; Adjusted di value
     mov [es:di], ax

	 ;-------------b2--------
mov di, 1708  ; Adjusted di value
bb0:
call Large_delay
  mov [es:di], ax
  add di, 264  ; Adjusting di value
  cmp di, 3028  ; Adjusting comparison value
  jne bb0

mov di, 1708  ; Adjusted di value
bb1:
call Large_delay
  mov [es:di], ax
  add di, 2  ; Adjusting di value
  cmp di, 1718  ; Adjusting comparison value
  jne bb1
 
 mov di, 2236  ; Adjusted di value
bb2:
call Large_delay
  mov [es:di], ax
  add di, 2  ; Adjusting di value
  cmp di, 2246  ; Adjusting comparison value
  jne bb2
  
 mov di, 2764  ; Adjusted di value
 bb3:
 call Large_delay
  mov [es:di], ax
  add di, 2  ; Adjusting di value
  cmp di, 2774  ; Adjusting comparison value
  jne bb3
 
 mov di, 1982  ; Adjusted di value
   mov [es:di], ax
   
   mov di, 2510  ; Adjusted di value
     mov [es:di], ax

; ; ;----- I --------------
mov di, 1722  ; Adjusted di value
l20:
call Large_delay
  mov [es:di], ax
  add di, 2  ; Adjusting di value
  cmp di, 1732  ; Adjusting comparison value
  jne l20
mov di, 2778  ; Adjusted di value
l21:
call Large_delay
  mov [es:di], ax
  add di, 2  ; Adjusting di value
  cmp di, 2788  ; Adjusting comparison value
  jne l21
mov di, 1726  ; Adjusted di value
l22:
call Large_delay
  mov [es:di], ax
  add di, 264  ; Adjusting di value
  cmp di, 3046  ; Adjusting comparison value
  jne l22

; ;--------------T-----------
mov di, 1736  ; Adjusted di value
l201:
call Large_delay
  mov [es:di], ax
  add di, 2  ; Adjusting di value
  cmp di, 1746  ; Adjusting comparison value
  jne l201
mov di, 2538 ; Adjusted di value
mov di,1740
l2211:
call Large_delay
  mov [es:di], ax
  add di, 264; Adjusting di value
  cmp di, 3060  ; Adjusting comparison value
  jne l2211

; ;----------- !! ----------------
; mov di,630
; l27:
; call Large_delay
  ; mov [es:di], ax
  ; add di,160
  ; cmp di,1110
  ; jne l27
; call Large_delay
; mov al, "@"
; mov ah,10110000b
; mov di,1270
; mov [es:di], ax

pop bp
pop di
pop si
pop dx
pop cx
pop bx
pop ax
pop es

pop ax
ret


delay21:
	; push cx
	; mov cx,0xffff
	; l011: loop l011
	; pop cx
	ret
Large_delay:
   call delay21
   call delay21
   ;call delay21
   ret
movement:  
		    push dx
			push bx
			push ax
  			
		    add word[colorChange],1
			cmp word[colorChange],4
			je clrchng
			jne clrchngAfter
			clrchng:
			mov word[colorChange],0
			
			clrchngAfter:
		    mov word[flag],1
    		
			mov ax,396
			push ax
			mov bx,38
			push bx
			call RestoreScreenFromBuffer
			
			
			mov ax,37
			push ax
			mov ax,131
			push ax
			mov ax,42
			push ax
			mov ax,131
			push ax
            call scrolldown
			
			
			
			mov ax,28
		    push ax
		    mov ax,0
		    push ax
		    mov ax, 33
		    push ax
		    mov ax, 132
		    push ax
	        call grass
		
		    mov ax,[upperBoardColor]
			mov bx,[midBoardColor]
           

            mov word[midBoardColor],ax
			mov word[belowBoardColor],bx
			
		    call newBoard
			
			
			mov bx,word[colorChange]
			cmp bx,1
			je printCarrot
			jne printCarrotAfter
			
			printCarrot:
			
			push 0xcccc
			push 55
			push 21
			call randomGenerate
			pop dx
			
			mov [carrotCoordinates+0],dx
			add dx,1
			mov [carrotCoordinates+2],dx
			
			
		    mov ax,29
            push ax
            mov ax,[carrotCoordinates+0]
            push ax
			mov ax,29
            push ax
            mov ax,[carrotCoordinates+2]
            push ax
            call carrot	
			
			printCarrotAfter:
			 
			call rabbits
			
				
			mov bx,word[colorChange]
			cmp bx,3
			je chekcar
			jne chekcarafter
			chekcar:
			mov ax,[carrotCoordinates+0]        ; 70
			cmp ax,[columnRabbitEnd]
			jl nextCheckCarrot
			jge checkJump
			
			nextCheckCarrot:
			
			mov ax,[carrotCoordinates+2]        ;71
			cmp ax,[columnRabbitStart]
			jg scoreADD
			jle checkJump
			
			scoreADD:
			add word[score],1
			
			push 0
			push 129
			push word[score]
			call printnum
			
			 
			chekcarafter:
			checkJump:
			
			mov ax,[changingBelowBoardLocation+0]
			cmp ax,[columnRabbitEnd]
			jl nextCheck
			jge gameOver
			nextCheck:
			
			mov ax,[changingBelowBoardLocation+2]
			cmp ax,[columnRabbitStart]
			jg endMovement
			jle gameOverCall
			
			gameOverCall:
			
			mov word[gameOverFlag],1
			
			endMovement:
			mov word[flag],0
			
			pop ax
			pop bx
			pop dx
			ret
			
scrolldown:	

            push bp
			mov bp,sp
			push ax
			push cx
			push si
			push di
			push es
			push ds

			mov ax, 132 ; load chars per row in ax
			mul byte [bp+10] ; calculate source position           132x43=3828
			add ax,[bp+8]
			mov si, ax ; load source position in si
			shl si, 1 ; convert to byte offset               11352
			

			mov cx, 1320 ; number of screen locations
			;sub cx, 132 ; count of words to move

			mov ax, 0xb800
			mov es, ax ; point es to video base
			mov ds, ax ; point ds to video base
		
			mov ax, 132 ; load chars per row in ax
			mul byte [bp+6] ; calculate source position           132x43=3828
			add ax,[bp+4]
			mov di, ax ; load source position in si
			shl di, 1 ; convert to byte offset               11352
			
			std ; set auto increment mode
			rep movsw     ;mov [es:di],[ds:si].... sub di,2...sub si,2.....dec cx(until cx=0)
			

			
			pop ds
			pop es
			pop di
			pop si
			pop cx
			pop ax
			pop bp
			ret 8			
	 			
SaveScreenToBuffer:

push bp
mov bp,sp
push ax
push ds
push es
push di
push si
push cx
;pusha
;push ax
mov cx, [bp+6]
mov ax,132
mul word[bp+4]
shl ax,1
mov si, ax               ;28 row starting column
mov di, buffer2                  ;buffer2 first cell address

mov ax, 0xb800
mov ds, ax
mov ax, 0x19f5
mov es, ax



cld
rep movsw        ;mov [es:di],[ds:si].... add di,2...add si,2.....dec cx(until cx=0)

pop cx
pop si
pop di
pop es
pop ds
pop ax
pop bp

ret 4



RestoreScreenFromBuffer:

push bp
mov bp,sp
push ax
push ds
push es
push di
push si
push cx
push bx


mov ax,132
mul word[bp+4]
shl ax,1
mov di, ax


mov ax, 0xb800
mov es, ax
mov ax, 0x19f5
mov ds, ax
mov si,buffer2



mov cx, [bp+6]
cld
rep movsw           ;mov [es:di],[ds:si].... add di,2...add si,2.....dec cx(until cx=0)




pop bx
pop cx
pop si
pop di
pop es
pop ds
pop ax
pop bp

ret 4

; resFlag: dw 0
; jumpFlag: dw 0


kbisr:		push bp
			mov bp, sp
			push ax
			push es
            push bx
			
			push cs
			pop ds
			
			; mov ax, 0xb800
			; mov es, ax									; point es to video memory


			;;; pn
			; push 0
			; push 0
			; push word[bp + 2]
			; call printnum
			 in al, 0x60									; read a char from keyboard port
		
			cmp al, 0x48					; is the up key
			je rabbitGame			; leave interrupt routine
			jne nextComparison        ; no, try next comparison
			
	nextComparison:
 
           	cmp al, 0x01
			je resumeScreen
			
			jne nomatch
			
			
    rabbitGame:			
			mov word[seconds],0
		    mov word[movementFlag],1
			jmp nomatch
	resumeScreen:
			mov word[cs:pauseFlag], 1


    nomatch:	; mov al, 0x20
			; out 0x20, al

			pop bx
			pop es
			
			; mov ah, 0x0e
			; mov al, 'x'
			; int 0x10
			
			pop ax
			pop bp
			jmp far [cs:oldisr] ; call the original ISR
		
			
exit:		mov al, 0x20
			out 0x20, al ; send EOI to PIC
			pop bx
			pop es
			pop ax
			pop bp
			iret ; return from interrupt			
			; iret 
			
timer:		push ax
        
			cmp byte[cs:flagTimer], 1     ; is the printing flag set
			jne skipall                   ; no, leave the ISR
			
			inc word [cs:seconds]        ; increment tick count
			cmp word[cs:seconds],100
			jnae skip_time
				mov word[gameOverFlag],1
		
				mov byte[cs:flagTimer], 0
				jmp skipall
			skip_time:
			 ; print tick count

			
			push 0
			push 50
			push word [cs:seconds]
			call printnum

skipall:	mov al, 0x20
			out 0x20, al ; send EOI to PIC
			
			
			pop ax
			iret ; return from interrupt			
;--------------------------------------------------------------------
; subroutine to call all the functions in the project
;--------------------------------------------------------------------						
printMainScreen:
 	 
	push ax
		push bx
		push cx
			 
			 mov ax, 1003h
      mov bx, 0 ; disable blinking.
      int 10h
			 
	
	    call clrscr              ; call the clrscr subroutine
		
		mov ax,[rows]               ;storing rows in ax
		mov bx, [colLength]            ;storing columns in bx
		mul bx                   ;multiply ax into bx and storing answer in ax
		sub ax, [colLength]            ;subtracting one column from total length as it is odd
		mov bx,3                 ;moving 3 in bx register
		div bx                   ;dividing 1/3rd the whole screen
		push ax                  ;pushing ax
		mov bx, 0x775f          ;pushing attribute and ascii
		push bx
		call division
		add ax,ax
		sub ax,264
		push ax
		mov ax, 0x775f
		push ax
		call division
	    
		mov ax,0xb0
		push ax
		mov ax,0
		push ax
		mov ax,13
		push ax
		mov ax,132
		push ax
		call sky
		call birds
		call mountains 	
		call trees
		call buildings
		
		mov ax, 15
		push ax
		mov ax, 0
		push ax
		mov ax, 26
		push ax
		mov ax, 132
		push ax
		call sea
        call boats
		
	    
		mov ax,28
		push ax
		mov ax,0
		push ax
		mov ax, 43
		push ax
		mov ax, 132
		push ax
	    call grass
		call boards
		
		
		mov ax,0
		push ax
		mov ax,127
		push ax
		call scoreBoard
		

		mov ax,396
		push ax
		mov ax,38
		push ax
		call SaveScreenToBuffer
		call rabbits
		
		pop cx
		pop bx
		pop ax
		
		ret
		
; ---------
printFlags:
		push ax
		push bx
		push cx
		
		mov ah, 0x0e

		mov bx, resumeflag
;resumeflag
;terminateFlag
;startFlag
;quitGameFlag
;gameOverFlag
;movementFlag
		mov cx, 6
.l2: 	
		mov al, '0'
		add al, [bx + 1]
		int 10h
		
		
		mov al, '0'
		add al, [bx]
		int 10h

		add bx, 2

		mov al, ';'
		int 10h
				
		loop .l2

		pop cx
		pop bx
		pop ax
		
		ret

;--------------------------------------------------------------------
start1:	
;code of high resolution , 43x132 
mov AH,0x00
mov al, 0x54
mov bh,0
int 0x10
		xor ax, ax
		mov es, ax										; point es to IVT base
			
		mov ax, [es:9*4]
		mov [oldisr], ax								; save offset of old routine
		mov ax, [es:9*4+2] 
		mov [oldisr+2], ax								; save segment of old routine
		
		mov ax,[es:8*4]
		mov [oldTimer],ax
		mov ax,[es:8*4+2]
		mov [oldTimer+2],ax
			
		cli												; disable interrupts
		mov word [es:9*4], kbisr						; store offset at n*4
		mov [es:9*4+2], cs								; store segment at n*4+2
	    mov word [es:8*4], timer ; store offset at n*4
		mov [es:8*4+2], cs ; store segment at n*4+
		sti


	; enable interrupts
start:
        ;call resumeScreenPrint
		mov word [columnRabbitStart],61
        mov word [columnRabbitEnd],68
		mov word [carrotCoordinates+0],60
		mov word [carrotCoordinates+2], 61
        mov word [originalBoardLocation+0],55
		mov word [originalBoardLocation+2], 75
        mov word [changingBelowBoardLocation+0],55
		mov word [changingBelowBoardLocation+2], 75
        mov word [changingupperBoardLocation+0],55
		mov word [changingupperBoardLocation+2],75
        mov word [changingRabbitBoardLocation+0],55
		mov word [changingRabbitBoardLocation+2],75
		mov word [colorChange],0
        mov word [upperBoardColor],0xc7
        mov word [midBoardColor],0xe7
        mov word [belowBoardColor],0xa7
		mov word [score],0
        mov word [seconds],0
		mov word [flag],0
        mov word [flagTimer],0
		mov word [resumeflag],0
        mov word [terminateFlag],0
        mov word [startFlag],0
        mov word [gameOverFlag],0
        mov word [movementFlag],0
        mov word [pauseFlag],0
		
		jmpyRabScrnPrint:
		
        call Print_JumpyRabbit
		
		lopInt16Loop:
		
		mov ah,0
		int 16h
		
		cmp al,'p'
		je PresspStart
		jne nextCompare
		
		PresspStart:
		jmp printMainScreenPrint
		
		nextCompare:
		cmp al,'e'
		je pressETerminate
		jne lopInt16Loop
		
		pressETerminate:
		jmp terminateProgram
		
		printMainScreenPrint:
		
        call printMainScreen
		       	   
		
		loop17:
		
		; mov ah,0eh
		; mov al,'o'
		; int 10h
		
			cmp word[pauseFlag], 0
			je startFlagCHeck
			call resumeScreenPrint
			
			mov word[pauseFlag], 0
			
			startFlagCHeck:
			cmp word[startFlag],0
			je terminateFlagCheck
			mov word[startFlag],0
			jmp start
			
			terminateFlagCheck:
			cmp word[terminateFlag],0
			je gameOverFlagCheck
			mov word[terminateFlag],0
			jmp terminateProgram
			
			gameOverFlagCheck:
			cmp word[gameOverFlag],0
			je movementFlagCheck
			mov word[gameOverFlag],0
			jmp gameOverCallInMain
			
			movementFlagCheck: 
			cmp word[movementFlag],0
			je resumeFlagcHeck
			mov word[movementFlag],0
			call movement
			jmp loop17
			
			resumeFlagcHeck:
			cmp word [resumeflag],0
			je anim
			mov word [resumeflag],0
			jmp printMainScreenPrint
			
			
            anim:
        call playAnimation
        jmp loop17                                      ;infinte loop
	
		gameOverCallInMain:
		call gameOver
		jmp loop17
		
        ;mov dx,start
		;add dx,15
		;mov cl,4
		;shr dx,cl
		
		;mov ax,0x3100
		;int 0x21
		
		terminateProgram:
		call quitGame
		

		cli
		mov ax, [oldisr]
		mov [es:9*4], ax
		mov ax, [oldisr+2]
		mov [es:9*4+2], ax
		sti
		
		xor ax,ax
		xor bx,bx
		mov ax,[oldTimer]
		mov bx,[oldTimer+2]

		cli ; disable interrupts
		mov word [es:8*4], ax ; store offset at n*4
		mov [es:8*4+2], bx ; store segment at n*4+
		sti ; enable interrupts	
			
		mov ah, 0
		mov al, 3
		int 0x10
		mov ax, 0x4c00
		int 0x21