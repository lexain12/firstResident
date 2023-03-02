.286
.model tiny
.code

org 100h

start: 
	cli
	xor bx, bx
	mov es, bx
	mov bx, 4*8

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

	cli
	push ax
	push bx
	push cx
	push dx
	push es
	push di
	pushf
	
	mov bx, 0b800h
	mov es, bx
	mov cx, 0
	mov bx, 0
	mov dl, 02h

	call ShowAxH


	popf
	pop di
	pop es
	pop dx
	pop cx
	pop bx
	pop ax
	sti


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

EOP:


end start

