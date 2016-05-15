blck(
	decls(
		[var(s)|
			decls(
				[
				proc(one, [], 
					blck([], 
						[
						write(num(5)), 
						retr(num(1))
						]
						)
					)
				]
				)
		]
		), 
	[
	read(var(s)), 
	if
	(
		var(s)+proc_call(one, []) > num(2), 
			[
			write(proc_call(one, []))
			]
	), 
	write(proc_call(one, []) - num(1))
	]
	) 