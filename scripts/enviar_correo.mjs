// Envia el correo diario del Pulso por Microsoft Graph (permiso delegado Mail.Send,
// misma app de Azure y mismo esquema de refresh token que usa el dashboard de la AFI).
// Lee la carpeta que genera scripts/correo_diario.R (asunto.txt, cuerpo.html,
// logo-pulso.png, meta.json). El unico adjunto es el logo en linea; el reporte es el link al Pulso. Por defecto es DRY-RUN: no envia nada.
//   env: SEND=true para enviar de verdad
//        AZURE_TENANT_ID, AZURE_CLIENT_ID, AZURE_REFRESH_TOKEN
//        CORREO_PARA (obligatorio) y CORREO_CC, separados por coma o punto y coma
//        CORREO_SOLO_A: si viene, envia SOLO a esa direccion (pruebas)
//        CORREO_AVISO: a quien avisar (solo Mario) si los datos estan atrasados; FORZAR=true envia igual
//   uso: node scripts/enviar_correo.mjs [carpeta]   (default: correo)
// Si el token se renueva, deja el nuevo en .new-refresh-token (el workflow lo guarda).
import { readFileSync, writeFileSync } from 'node:fs';
import { join } from 'node:path';

const dir = process.argv[2] || 'correo';
const env = process.env;
const lista = (s = '') => s.split(/[;,\n]/).map((x) => x.trim()).filter(Boolean);
const LOGIN = env.LOGIN_BASE || 'https://login.microsoftonline.com';
const GRAPH = env.GRAPH_BASE || 'https://graph.microsoft.com/v1.0';

const asunto = readFileSync(join(dir, 'asunto.txt'), 'utf8').trim();
const html = readFileSync(join(dir, 'cuerpo.html'), 'utf8');
const meta = JSON.parse(readFileSync(join(dir, 'meta.json'), 'utf8'));
const para = env.CORREO_SOLO_A ? lista(env.CORREO_SOLO_A) : lista(env.CORREO_PARA);
const cc = env.CORREO_SOLO_A ? [] : lista(env.CORREO_CC);
if (!para.length) { console.error('Falta CORREO_PARA (o CORREO_SOLO_A).'); process.exit(1); }
const enviar = env.SEND === 'true';

const adjuntos = [
  { '@odata.type': '#microsoft.graph.fileAttachment', name: 'logo-pulso.png', contentType: 'image/png', contentId: 'logo-pulso',
    isInline: true, contentBytes: readFileSync(join(dir, 'logo-pulso.png')).toString('base64') },
];
const dest = (l) => l.map((address) => ({ emailAddress: { address } }));
const mensaje = { subject: asunto, importance: 'normal', body: { contentType: 'HTML', content: html },
                  toRecipients: dest(para), ccRecipients: dest(cc), attachments: adjuntos };

// Datos atrasados (el cierre es anterior al ultimo dia habil): NO se envia al equipo. Se avisa
// solo a CORREO_AVISO (Mario) con el motivo. FORZAR=true envia igual.
const atrasado = meta.atrasado === true && env.FORZAR !== 'true' && !env.CORREO_SOLO_A;
if (atrasado) {
  const aviso = lista(env.CORREO_AVISO);
  const motivo = [].concat(meta.avisos || []).join(' ') || 'El cierre es anterior al ultimo dia habil.';
  const run = env.GITHUB_SERVER_URL && env.GITHUB_RUN_ID ? `${env.GITHUB_SERVER_URL}/${env.GITHUB_REPOSITORY}/actions/runs/${env.GITHUB_RUN_ID}` : '';
  console.log(`DATOS ATRASADOS: no se envia al equipo. ${motivo}`);
  if (!enviar) process.exit(0);
  if (!aviso.length) { console.error('Datos atrasados y falta CORREO_AVISO: nadie fue avisado.'); process.exit(1); }
  const texto = `${motivo}\n\nNo se envio el Pulso de hoy al equipo. Cuando los datos esten al dia, vuelve a correr "Correo diario Pulso" (enviar = true)` +
                ` o usa forzar = true para enviarlo igual.${run ? `\n\nCorrida: ${run}` : ''}`;
  mensaje.subject = 'AVISO Pulso: no se envio el correo de hoy (datos atrasados)';
  mensaje.body = { contentType: 'Text', content: texto };
  mensaje.toRecipients = dest(aviso); mensaje.ccRecipients = []; mensaje.attachments = [];
}

console.log(`${enviar ? 'ENVIO REAL' : 'DRY-RUN (no se envia)'} | ${mensaje.subject}`);
console.log(`  para: ${mensaje.toRecipients.map((x) => x.emailAddress.address).join(', ')}${mensaje.ccRecipients.length ? ` | cc: ${mensaje.ccRecipients.map((x) => x.emailAddress.address).join(', ')}` : ''}`);
console.log(`  cierre: ${meta.cierre} | sin adjuntos (solo el logo en linea)`);
if (meta.avisos?.length) console.log(`  avisos: ${[].concat(meta.avisos).join(' | ')}`);
if (!enviar) process.exit(0);

for (const k of ['AZURE_TENANT_ID', 'AZURE_CLIENT_ID', 'AZURE_REFRESH_TOKEN'])
  if (!env[k]) { console.error(`Falta ${k}.`); process.exit(1); }

const tr = await fetch(`${LOGIN}/${env.AZURE_TENANT_ID}/oauth2/v2.0/token`, {
  method: 'POST', headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
  body: new URLSearchParams({ grant_type: 'refresh_token', client_id: env.AZURE_CLIENT_ID,
    refresh_token: env.AZURE_REFRESH_TOKEN, scope: 'https://graph.microsoft.com/Mail.Send offline_access' }) });
const tj = await tr.json();
if (!tr.ok) { console.error(`Token: ${tj.error} · ${String(tj.error_description).split('\r')[0]}`); process.exit(1); }
if (tj.refresh_token) writeFileSync('.new-refresh-token', tj.refresh_token, { mode: 0o600 });

const r = await fetch(`${GRAPH}/me/sendMail`, { method: 'POST',
  headers: { Authorization: `Bearer ${tj.access_token}`, 'Content-Type': 'application/json' },
  body: JSON.stringify({ message: mensaje, saveToSentItems: true }) });
if (!r.ok) { console.error(`sendMail ${r.status}: ${(await r.text()).slice(0, 300)}`); process.exit(1); }
console.log('Correo enviado.');
