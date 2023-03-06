locals @@
.286
.model tiny
.code

org 100h

;------------------------------------------------
x = 74
y = 1
;------------------------------------------------

start: 
	jmp EOP

New09 		proc

	push ax

	in al, 60h 				; waiting only for hotkey 
	cmp al, 0dh

	pop ax
	jne @@NotHotKey

	cmp cs:[Flag], 00h
	je @@NotAlreadyOn

	pushf
	push di 
	push si
	push ax
	push bx
	push es
	push cx

	mov di, offset SaveBuf 			; recover video segment
	mov bx, 0b800h
	mov es, bx
	mov si, (80d * y + x) * 2
	mov ax, 6d
	mov bx, 6d
	call FromBufToVid

	pop cx
	pop es
	pop bx 
	pop ax
	pop si
	pop di
	popf 					; why?

	@@NotAlreadyOn:
	xor cs:[Flag], 01h

	pushf
	pusha
	push es

	mov bx, 0b800h 				; save first frame 
	mov es, bx
	mov ax, 6d
	mov bx, 6d
	mov si, (80d * y + x) * 2
	mov di, offset SaveBuf
	call FromVidToBuf

	mov si, (80d * y + x) * 2 
	mov di, offset DrawBuf
	mov ax, 6d 				; Width 
	mov bx, 6d 				; Height
	call FromVidToBuf

	pop es
	popa
	popf

	push ax
	in al, 61h 				; talking to PPI
	or al, 80h
	out 61h, al 
	and al, not 80h
	out 61h, al

	mov al, 20h 				; talking to int controller
	out 20h, al

	pop ax
	
	iret

@@NotHotKey:
	db 0eah
	Old09offset dw 0
	Old09Seg    dw 0

New08  		proc

	cmp cs:[Flag], 00h
	je @@TurnedOff

	pusha 					; save 
	push ds
	push es

	push dx 				; for printing a registers
	push cx
	push bx
	push ax

	mov bx, 0b800h 				; params for ax reg
	mov es, bx
	mov si, (80d * y + x) * 2
	call Comparasion

	

;------Draw a table with regs--------------------


	pop ax
	mov cx, x + 01d
	mov bx, y + 01d
	mov dl, 02h

	call ShowAxH

	pop bx
	mov cx, x + 01d				; params for bx reg
	mov ax, bx
	mov bx, y + 02d
	mov dl, 02h

	call ShowAxH

	pop cx
	mov ax, cx
	mov cx, x + 01d
	mov bx, y + 03d
	mov dl, 02h

	call ShowAxH

	pop dx
	mov ax, dx
	mov cx, x + 01d
	mov bx, y + 04d
	mov dl, 02h

	call ShowAxH

	mov ah, x 
	mov al, y 
	mov bh, 6d
	mov bl, 6d

	call DrawTbl
;------------------------------------------------
	mov si, (80d * y + x) * 2
	mov di, offset DrawBuf
	mov ax, 6d 				; Width 
	mov bx, 6d 				; Height
	call FromVidToBuf

	
	pop es
	pop ds
	popa

	

@@TurnedOff: 					
	db 0eah 				; jmp 
	Old08offset dw 0
	Old08Seg    dw 0
endp

	endp
;---------------Data-----------------------------
Flag db 0h
SaveBuf db 72 dup (0)
DrawBuf db 72 dup (0)
;---------------End of data----------------------




;-----------------------------------------------
; Show the ax register in hex on screen (x, y) pos
;------------------------------------------------
; Entry: 	cx = x, 
; 		bx = y, 
; 		ax = number, 
;   		dl = color
; Exit: 	None
; Expects: 	es = 0b800h
; Desroys: 	bx, cx 
;------------------------------------------------
ShowAxH 		proc
	
	push ax 				;  

	mov ax, bx 				;--------------------------------
	mov bx, 80d  				; Calculation of pos (x, y) in di
	mul bl 					;
	add ax, cx 				;
	shl ax, 1 				;
	mov di, ax 				;--------------------------------

	pop ax
	push ax

	xor bx, bx 				
	xor cx, cx 				

	mov cl, 04h 				; for 4 iterations 

??loop1:
	mov bx, ax 
	and bx, 0f000h
	shr bx, 12
	cmp bx, 0ah;
	jae ??letter	
	jmp ??number

??letter:
	add bl, 55d 				; calculate ascii code of letter, 55 + 10 = 65: 'A'
	mov byte ptr es:[di], bl 		; put letter on screen
	inc di
	mov byte ptr es:[di], dl 		; put color for this letter
	inc di

	jmp ??countinue 

??number:
	add bl, 30h
	mov byte ptr es:[di], bl 		; put number on screen
	inc di
	mov byte ptr es:[di], dl 		; put color for this number
	inc di

	jmp ??countinue 


??countinue:
	shl ax, 4

	loop ??loop1
	jmp ??done


??done:

	pop ax
	ret
	endp
;------------------------------------------------

;------------------------------------------------
; Draws horizontal line from (x, y) to (x + length, y)
;------------------------------------------------
; Entry: 	ah = x, al = y, bh = length
; Exit: 	none
; Expects: 	Es = 0b800h
; Destroys: 	cx, di
;------------------------------------------------
DrawHzlLn 		proc

	xor cx, cx

	mov cl, ah
	mov ah, 80d
	mul ah
	add ax, cx
	mov di, ax 				; calculate (x,y) in di
	shl di, 1
	xor cx, cx
	mov cl, bh

@@line:

	mov es:[di], 02c4h 			; puts right symbols in videosegment
	inc di
	inc di 
	loop @@line
		

	ret	
	endp
;------------------------------------------------

;------------------------------------------------
; Draw a vertical line from (x, y) to (x, y + height)
;------------------------------------------------
; Entry: 	ah = x, al = y, bl = height
; Exit: 	None
; Expects: 	es = 0b8000
; Destoys
;------------------------------------------------
DrawVrcLn 	proc 

	mov cl, ah
	mov ah, 80d
	mul ah
	add ax, cx
	mov di, ax 				; calculate (x,y) in di
	shl di, 1

	xor cx, cx
	mov cl, bl
	dec cl
		
@@line: 	
	mov es:[di], 02b3h 			; puts symbols in videosegment
	add di, 160d
	loop @@line
	
	ret
	endp
;------------------------------------------------

;------------------------------------------------
; Set corners
; brief: Set corners of the table. x, y  -
; cordinates of left upper corner
;------------------------------------------------
; Entry: 	ah = x, al = y, bh = width, bl = height
; Exit: 	none
; Expects: 	ES = 0b800h
; Destroys:  	ax, cx, di
;------------------------------------------------
DrawCorners 		proc
		
			mov cl, ah
			mov ah, 80d
			mul ah
			add ax, cx
			mov di, ax
			shl di, 1
			push di 		; calculate (x,y) in di

			mov es:[di], 02dah 	; top left corner

			mov al, bh 
			shl al, 1
			xor ah, ah
			add di, ax 		; calculate shift in x
			dec di
			dec di
			mov es:[di], 02bfh 	; top right corner

			mov al, bl
			mov cx, 80d
			xor ah, ah
			dec al
			mul cx
			shl ax, 1	
			add di, ax 		; calulate shift in y
			mov es:[di], 02d9h 	; bottom right corner 

			pop di
			add di, ax
			mov es:[di], 02c0h 	; bottom left corner


			ret
			endp
;------------------------------------------------


;------------------------------------------------
; Draw a table. Top left corner in (x, y)
;------------------------------------------------
; Entry: 	ah = x, al = y, bh = widht, bl = height
; Exit: 	None
; Expects: 	es = 0b800
; Destroys: 	
;------------------------------------------------
DrawTbl 	proc

	push ax 				; arguments for function
	push bx
	call DrawCorners
	
	pop bx 					; arguments for function
	pop ax 
	push ax
	push bx

	inc ah
	dec bh
	dec bh

	call DrawHzlLn
	
	pop bx 					; correct arguments for function 
	pop ax 
	push ax
	push bx

	add al, bl
	dec al
	inc ah
	dec bh
	dec bh
	call DrawHzlLn

	pop bx 					; correct arguments for function
	pop ax 
	push ax
	push bx

	inc al 
	dec bl
	call DrawVrcLn

	pop bx 					; correct arguments for function
	pop ax 

	inc al
	dec bl
	add ah, bh
	dec ah
	call DrawVrcLn
		
	
	ret
	endp
;------------------------------------------------

;------------------------------------------------
; Read from video segment to buf 
;------------------------------------------------
; Entry: si = addres on videosegment, di = addres on buf, ax = width, bx = height
; Exit: none
; Expects: es = 0b800h
; Destroys: cx, bx
;------------------------------------------------
FromVidToBuf 		proc	
		
@@Coping1: 					; copy lines

	mov cx, ax
	
	push ax
	@@Coping: 				; copying one line
		mov ax, es:[si]
		mov cs:[di], ax
		inc si
		inc si 
		inc di 
		inc di
		loop @@Coping

	pop ax
	shl ax, 1
	sub si, ax
	shr ax, 1

	add si, 160d
	dec bx
	cmp bx, 0h
	jne @@Coping1
	

ret 
endp
;------------------------------------------------

;------------------------------------------------
; Read from buf to videosegment
;------------------------------------------------
; Entry: si = address on videosegment, di = addres on buf, ax = width, bx = height
; Exit: none
; Expects: es = 0b800h
; Destroys: cx, bx
;------------------------------------------------
FromBufToVid 		proc	
		
@@Coping1: 					; copy lines

	mov cx, ax
	push ax
	@@Coping: 				; copying one line
		mov ax, cs:[di]
		mov es:[si], ax
		inc si
		inc si 
		inc di 
		inc di
		loop @@Coping

	pop ax
	shl ax, 1
	sub si, ax
	shr ax, 1

	add si, 160d
	dec bx
	cmp bx, 0h
	jne @@Coping1
	

ret 
endp
;------------------------------------------------

;------------------------------------------------
; Compares videosegment with buf0 and puts in buf1 differences from buf0
;------------------------------------------------
; Entry: si = adress on videosegment
; Destroys: ax
;------------------------------------------------
Comparasion 		proc
	
	mov ax, 0b800h
	mov es, ax
	xor ax, ax
	mov cx, 6d 				; height
	xor bp, bp

@@Loop1:
	xor bx, bx
	push si
	@@Loop2:
		mov al, es:[si]
		cmp al, cs:DrawBuf[bp]
		je @@AllOk
		mov cs:SaveBuf[bp], al
	@@AllOk:
		inc bp
		inc bx
		inc si

		cmp bx, 12d
		jne @@Loop2

	pop si
	add si, 160d
	loop @@Loop1

ret 
endp
;------------------------------------------------

EOP:

	xor ax, ax
	xor bx, bx
	xor cx, cx
	xor dx, dx
	xor si, si
	xor di, di 
	xor bp, bp

	cli 					; change 08 int 
	mov es, bx
	mov bx, 4*8

	mov ax, es:[bx]
	mov Old08offset, ax
	mov es:[bx], offset New08

	mov ax, es:[bx+2]
	mov Old08Seg, ax
	mov ax, cs
	mov es:[bx+2], ax


	mov bx, 4*9  				; change 09 int

	mov ax, es:[bx]
	mov Old09offset, ax
	mov es:[bx], offset New09

	mov ax, es:[bx+2]
	mov Old09Seg, ax
	mov ax, cs
	mov es:[bx+2], ax
	sti

	mov ax, 3100h
	mov dx, offset EOP
	shr dx, 4
	inc dx
	int 21h


end start 


