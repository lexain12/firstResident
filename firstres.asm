locals ??
.286
.model tiny
.code

org 100h

;------------------------------------------------
x = 75
y = 1
;------------------------------------------------

start: 
	cli
	xor bx, bx
	mov es, bx
	mov bx, 4*9

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


New09 		proc

	push ax
	push bx
	push cx
	push dx
	push es
	push di
	
	push dx
	push cx
	push bx
	push ax

	in al, 60h
	cmp al, 3ah
	jne commonKey

	pop ax
	mov bx, 0b800h 				; params for ax reg
	mov es, bx
	mov cx, 75d
	mov bx, y
	mov dl, 02h

	call ShowAxH

	pop bx
	mov cx, 75d 				; params for bx reg
	mov ax, bx
	mov bx, y + 1d
	mov dl, 02h

	call ShowAxH

	pop cx
	mov ax, cx
	mov cx, x
	mov bx, y + 2d
	mov dl, 02h

	call ShowAxH

	pop dx
	mov ax, dx
	mov cx, x
	mov bx, y + 03d
	mov dl, 02h

	call ShowAxH

	mov ah, x - 1d
	mov al, y - 1d
	mov bh, 6d
	mov bl, 6d

	call DrawTbl
	
	in al, 61h 				; talking to PPI
	or al, 80h
	out 61h, al 
	and al, not 80h
	out 61h, al

	mov al, 20h 				; talking to int controller
	out 20h, al

	pop di
	pop es
	pop dx
	pop cx
	pop bx
	pop ax
	iret


commonKey:
	pop ax
	pop bx
	pop cx
	pop dx

	pop di
	pop es
	pop dx
	pop cx
	pop bx
	pop ax

	db 0eah
	Old09offset dw 0
	Old09Seg    dw 0
	endp


;------------------------------------------------
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

??line:

	mov es:[di], 02c4h 			; puts right symbols in videosegment
	inc di
	inc di 
	loop ??line
		

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
		
??line: 	
	mov es:[di], 02b3h 			; puts symbols in videosegment
	add di, 160d
	loop ??line
	
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

EOP:


end start

