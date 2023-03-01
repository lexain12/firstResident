.286
.model tiny
.code

org 100h

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
	push es

	mov bx, 0b800h
	mov es, bx
	mov ah, 4eh
	xor bx, bx

	in al, 60h
	mov bl, al
	shl bl, 1
	mov es:[bx], ax

	pop es
	pop bx
	pop ax


	db 0eah
	Old09offset dw 0
	Old09Seg    dw 0
	endp

EOP:


end start
