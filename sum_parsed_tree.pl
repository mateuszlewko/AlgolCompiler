% v1
blck(
	decl([
			[var(x), var(s)]
		]), 
	instr(
		assgn(
			var(s), num(0)
			), 
		instr(
			read(var(x)),

			instr(
				while(
					<>(atmic(var(x)), num(0)), 

					instr(
						assgn(var(s), atmic(var(s))+atmic(var(x))
							), 
						instr(
							read(var(x))
							)
						)
					), 
				instr(
					write(
						atmic(var(s))
						)
					 )
				)
			)
		)
	).

% v2
blck(
	decls(
		[var(x)]
		), 
	instr(
		assgn(var(s), num(0)), 
		instr(
			read(var(x)), 
			instr(
				while(<>(var(x), num(0)), 
					instr(
						assgn(var(s),
						 	num(4)+var(x)),
						instr(
							read(var(x))
							)
						)
					),
				instr(
					write(var(s))
					)
				)
			)
		)
	).

% v3
blck(
	decls(
		[var(x)]
		), 
	[
	assgn(var(s), num(0)), 
	read(var(x)), 
	while(
		<>(var(x), num(0)), 
		[
			assgn(var(s), 
				num(4)+var(x)),
			instr(
				read(var(x))
				 )
		]
		), 
	instr(
		write(var(s))
		)
	]
	)