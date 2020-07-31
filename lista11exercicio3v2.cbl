      $set sourceformat"free"
      *>Divisão de identificação do programa
       identification division.
       program-id. "lista11exercicio3v2".
       author. "Débora Reinert".
       installation. "PC".
       date-written. 29/07/2020.
       date-compiled. 29/07/2020.



      *>Divisão para configuração do ambiente
       environment division.
       configuration section.
           special-names. decimal-point is comma.

      *>-----Declaração dos recursos externos
       input-output section.
       file-control.

           select arqCadAlunos assign to "arqCadAlunos.dat"
           organization is indexed
           access mode is dynamic
           lock mode is automatic
           record key is fd-codigo
           file status is ws-fs-arqCadAlunos.



       i-o-control.

      *>Declaração de variáveis
       data division.

      *>----Variaveis de arquivos
       file section.
       fd  arqCadAlunos.
       01  fd-alunos.
           05  fd-codigo                           pic 9(03).
           05  fd-aluno                            pic x(25).
           05  fd-endereco                         pic x(35).
           05  fd-mae                              pic x(25).
           05  fd-pai                              pic x(25).
           05  fd-telefone                         pic x(15).
           05  fd-nota-g.
               10  fd-notas occurs 4.
                   15 fd-nota                      pic 9(02)v99.

      *>----Variaveis de trabalho
       working-storage section.

       77  ws-fs-arqCadAlunos                         pic 9(02).


       01  ws-alunos.
           05  ws-codigo                           pic 9(03).
           05  ws-aluno                            pic x(25).
           05  ws-endereco                         pic x(35).
           05  ws-mae                              pic x(25).
           05  ws-pai                              pic x(25).
           05  ws-telefone                         pic x(15).
           05  ws-nota-g.
               10  ws-notas occurs 4.
                   15  ws-nota                     pic 9(02)v99.

       01  ws-diversos.
           05  ws-sair                             pic x(01).
           05  ws-menu                             pic x(01).
           05  ws-ind                              pic 9(01).

       01 ws-msn-erro.
          05 ws-msn-erro-ofsset                    pic 9(04).
          05 filler                                pic x(01) value "-".
          05 ws-msn-erro-cod                       pic 9(02).
          05 filler                                pic x(01) value space.
          05 ws-msn-erro-text                      pic x(42).



      *>----Variaveis para comunicação entre programas
       linkage section.


      *>----Declaração de tela
       screen section.


      *>Declaração do corpo do programa
       procedure division.


           perform inicializa.
           perform processamento.
           perform finaliza.

      *>------------------------------------------------------------------------
      *>  Inicialização
      *>------------------------------------------------------------------------
       inicializa section.

           open i-o arqCadAlunos
           if ws-fs-arqCadAlunos  <> 00
           and ws-fs-arqCadAlunos <> 05 then
               move 1                                         to ws-msn-erro-ofsset
               move ws-fs-arqCadAlunos                        to ws-msn-erro-cod
               move "Erro ao abrir arquivo: (arqCadAlunos) "  to ws-msn-erro-text
               perform finaliza-anormal
           end-if


           .
       inicializa-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Processamento
      *>------------------------------------------------------------------------
       processamento section.

           perform until ws-sair = "S"
                      or ws-sair = "s"

               display erase
               display "Digite (1)- Novo Cadastro de Aluno"
               display "Digite (2)- Registro de Notas"
               display "Digite (3)- Consulta Cadastro"
               display "Digite (4)- Alteracao de Cadastro"
               display "Digite (5)- Exclusão de Cadastro"
               accept ws-menu

               evaluate ws-menu
                   when = "1"
                       perform cadastrar-aluno

                   when = "2"
                       perform cadastrar-notas

                   when = "3"
                       perform consultar-cadastro

                   when = "4"
                       perform alterar-cadastro

                   when = "5"
                       perform deletar-cadastro

                   when other
                       display "Opcao inexistente."

               end-evaluate

               display "Informe (S) para sair ou (C) para continuar."
               accept ws-sair

           end-perform


           .
       processamento-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Cadastro de aluno
      *>------------------------------------------------------------------------
       cadastrar-aluno section.

           display "Informe o codigo do aluno: "
           accept ws-codigo
           display "Informe o nome do aluno: "
           accept ws-aluno
           display "Informe o endereco do aluno: "
           accept ws-endereco
           display "Informe o nome da mae do aluno: "
           accept ws-mae
           display "Informe o nome do pai do aluno: "
           accept ws-pai
           display "Informe o telefone para contato: "
           accept ws-telefone

           write fd-alunos   from   ws-alunos
           if ws-fs-arqCadAlunos  <> 00 then
               move 1                                            to ws-msn-erro-ofsset
               move ws-fs-arqCadAlunos                           to ws-msn-erro-cod
               move "Erro ao gravar arquivo: (arqCadAlunos). "   to ws-msn-erro-text
               perform finaliza-anormal
           end-if

           .
       cadastrar-aluno-exit.
           exit.


      *>------------------------------------------------------------------------
      *>  Cadastro de notas
      *>------------------------------------------------------------------------
       cadastrar-notas section.

           display "Codigo do aluno: "
           accept ws-codigo

           display "Nota 1: "
           accept ws-nota(1)

           display "Nota 2: "
           accept ws-nota(2)

           display "Nota 3: "
           accept ws-nota(3)

           display "Nota 4: "
           accept ws-nota(4)

           move ws-codigo     to    fd-codigo
           read arqCadAlunos
           if ws-fs-arqCadAlunos  <> 00 then
               if ws-fs-arqCadAlunos = 23 then
                   display "Codido de aluno inexistente."
               else
                   move 1                                           to ws-msn-erro-ofsset
                   move ws-fs-arqCadAlunos                          to ws-msn-erro-cod
                   move "Erro ao ler arquivo: (arqCadAlunos)."      to ws-msn-erro-text
                   perform finaliza-anormal
               end-if
           else
               move ws-nota-g   to    fd-nota-g
               rewrite fd-alunos
               if ws-fs-arqCadAlunos  <> 00 then
                   move 1                                                    to ws-msn-erro-ofsset
                   move ws-fs-arqCadAlunos                                   to ws-msn-erro-cod
                   move "Erro ao gravar notas no arquivo: (arqCadAlunos)."   to ws-msn-erro-text
                   perform finaliza-anormal
               end-if
           end-if

           .
       cadastrar-notas-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Consulta de cadastro
      *>------------------------------------------------------------------------
       consultar-cadastro section.

           display "Digite (I) para consulta indexada"
           display "Digite (S) para consulta sequencial"
           accept ws-menu

           evaluate ws-menu
               when = "I" or "i"
                   perform consulta-ind

               when = "S" or "s"
                   perform consulta-seq-next

               when other
                  display "Opcao inexistente."
           end-evaluate


           .
       consultar-cadastro-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Consulta indexada
      *>-----------------------------------------------------------------------
       consulta-ind section.

           display "Informe o codigo do aluno: "
           accept ws-codigo

           move ws-codigo         to    fd-codigo
           read arqCadAlunos
           if ws-fs-arqCadAlunos  <> 00 then
               if ws-fs-arqCadAlunos = 23 then
                   display "Codido de aluno inexistente."
               else
                   move 1                                           to ws-msn-erro-ofsset
                   move ws-fs-arqCadAlunos                          to ws-msn-erro-cod
                   move "Erro ao ler arquivo: (arqCadAlunos)."      to ws-msn-erro-text
                   perform finaliza-anormal
               end-if
           else
               move fd-alunos     to   ws-alunos

               display "Aluno   : "    ws-aluno
               display "Mae     : "    ws-mae
               display "Pai     : "    ws-pai
               display "Endereco: "    ws-endereco
               display "Telefone: "    ws-telefone
               display "Nota 1  : "    ws-nota(1)
               display "Nota 2  : "    ws-nota(2)
               display "Nota 3  : "    ws-nota(3)
               display "Nota 4  : "    ws-nota(4)
           end-if

           .
       consulta-ind-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Consulta sequencial
      *>-----------------------------------------------------------------------
       consulta-seq-next section.

           perform until ws-sair = "N"
                      or ws-sair = "n"
               read arqCadAlunos next
               if ws-fs-arqCadAlunos  <> 00 then
                   if ws-fs-arqCadAlunos = 10 then
                       perform consulta-seq-prev
                   else
                       move 1                                            to ws-msn-erro-ofsset
                       move ws-fs-arqCadAlunos                           to ws-msn-erro-cod
                       move "Erro ao ler arquivo: (arqCadAlunos). "      to ws-msn-erro-text
                       perform finaliza-anormal
                   end-if
               else
                   move fd-alunos     to   ws-alunos

                   display "Aluno   : "    ws-aluno
                   display "Mae     : "    ws-mae
                   display "Pai     : "    ws-pai
                   display "Endereco: "    ws-endereco
                   display "Telefone: "    ws-telefone
                   display "Nota 1  : "    ws-nota(1)
                   display "Nota 2  : "    ws-nota(2)
                   display "Nota 3  : "    ws-nota(3)
                   display "Nota 4  : "    ws-nota(4)
               end-if

               display "Deseja ler o proximo cadastro? Digite (S) para sim ou (N) para nao."
               accept ws-sair
           end-perform


           .
       consulta-seq-next-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Consulta sequencial com previous
      *>-----------------------------------------------------------------------
       consulta-seq-prev section.

           perform until ws-sair = "N"
                      or ws-sair = "n"
               read arqCadAlunos previous
               if ws-fs-arqCadAlunos  <> 00 then
                   if ws-fs-arqCadAlunos = 10 then
                       perform consulta-seq-next
                   else
                       move 1                                          to ws-msn-erro-ofsset
                       move ws-fs-arqCadAlunos                         to ws-msn-erro-cod
                       move "Erro ao ler arquivo (arqCadAlunos)."      to ws-msn-erro-text
                       perform finaliza-anormal
                   end-if
               else
                   move fd-alunos     to   ws-alunos

                   display "Aluno   : "    ws-aluno
                   display "Mae     : "    ws-mae
                   display "Pai     : "    ws-pai
                   display "Endereco: "    ws-endereco
                   display "Telefone: "    ws-telefone
                   display "Nota 1  : "    ws-nota(1)
                   display "Nota 2  : "    ws-nota(2)
                   display "Nota 3  : "    ws-nota(3)
                   display "Nota 4  : "    ws-nota(4)
               end-if

               display "Deseja ler o proximo cadastro? Digite (S) para sim ou (N) para nao."
               accept ws-sair
           end-perform


           .
       consulta-seq-prev-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Deleta o cadastro
      *>-----------------------------------------------------------------------
       deletar-cadastro section.
           display "Informe o codigo do aluno: "
           accept ws-codigo

           move ws-codigo         to    fd-codigo
           delete arqCadAlunos
           if ws-fs-arqCadAlunos  <> 00 then
               if ws-fs-arqCadAlunos = 23 then
                   display "Codido de aluno inexistente."
               else
                   move 1                                            to ws-msn-erro-ofsset
                   move ws-fs-arqCadAlunos                           to ws-msn-erro-cod
                   move "Erro ao deletar arquivo: (arqCadAlunos). "  to ws-msn-erro-text
                   perform finaliza-anormal
               end-if
           end-if
           .
       deletar-cadastro-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Alteração de cadastro
      *>-----------------------------------------------------------------------
       alterar-cadastro section.

           display "Informe o codigo do aluno: "
           accept ws-codigo

           move ws-codigo    to   fd-codigo
           read arqCadAlunos
           if ws-fs-arqCadAlunos  <> 00 then
               if ws-fs-arqCadAlunos = 23 then
                   display "Codido de aluno inexistente."
               else
                   move 1                                           to ws-msn-erro-ofsset
                   move ws-fs-arqCadAlunos                             to ws-msn-erro-cod
                   move "Erro ao ler arquivo: (arqCadAlunos) "      to ws-msn-erro-text
                   perform finaliza-anormal
               end-if
           else

               move fd-alunos     to    ws-alunos

               display " Digite (1) para aluno"
               display " Digite (2) para endereco"
               display " Digite (3) para nome da mae"
               display " Digite (4) para nome do pai"
               display " Digite (5) para telefone"
               display " Digite (6) para notas"

               accept ws-menu

               evaluate ws-menu
                   when = "1"
                       display "Nome do Aluno"
                       accept ws-aluno

                   when = "2"
                       display "Endereco"
                       accept ws-endereco

                   when = "3"
                       display "Nome Mae"
                       accept ws-mae

                   when = "4"
                       display "Nome Pai"
                       accept ws-pai

                   when = "5"
                       display "Telefone "
                       accept ws-telefone

                   when = "6"
                       display "Digite a nota (1-2-3-4)?"
                       accept ws-ind
                       display "Nota : "
                       accept ws-nota(ws-ind)

                   when other
                       display "Opcao inexistente"

               end-evaluate

               move ws-alunos to fd-alunos

               rewrite fd-alunos
               if ws-fs-arqCadAlunos  <> 00 then
                   move 1                                                 to ws-msn-erro-ofsset
                   move ws-fs-arqCadAlunos                                to ws-msn-erro-cod
                   move "Erro ao gravar notas arquivo: (arqCadAlunos) "   to ws-msn-erro-text
                   perform finaliza-anormal
               end-if


           end-if


           .
       alterar-cadastro-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Finaliza anormal
      *>------------------------------------------------------------------------
       finaliza-anormal section.
           display erase
           display ws-msn-erro.
           Stop run
           .
       finaliza-anormal-exit.
           exit.


      *>------------------------------------------------------------------------
      *>  Finalização
      *>------------------------------------------------------------------------
       finaliza section.

           close arqCadAlunos
           if ws-fs-arqCadAlunos  <> 00 then
               move 1                                           to ws-msn-erro-ofsset
               move ws-fs-arqCadAlunos                          to ws-msn-erro-cod
               move "Erro ao fechar arquivo: (arqCadAlunos) "   to ws-msn-erro-text
               perform finaliza-anormal
           end-if


           Stop run
           .
       finaliza-exit.
           exit.













